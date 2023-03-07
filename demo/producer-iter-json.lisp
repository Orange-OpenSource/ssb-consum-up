;;;; Copyright (c) 2022-2023 Orange. All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
;;;;     1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
;;;;     2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
;;;;     3. All advertising materials mentioning features or use of this software must display the following acknowledgement:
;;;;     This product includes software developed by Orange.
;;;;     4. Neither the name of Orange nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY Orange "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Orange BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;
;;;; This file is part of the SSB-CONSUM-UP project: KAFKA JSON-LD producer example

;;;; File template source: https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/

;;; Interactive usage:
;;; * Call (main (list "-i" "5"))
;;; * See also (produce 10) for direct evaluation.
;;; CLI usage:
;;; * sbcl --noinform --non-interactive --quit --load ./producer-iter-json.lisp --end-toplevel-options "--iter=80"

(ql:quickload '(cl-rdkafka babel unix-opts))

;;; cl-json-ld: first git clone the project from https://github.com/RDProjekt/cl-json-ld
;;; then adjust and evaluate the two following line
(push #p"./lib/cl-json-ld/" asdf:*central-registry*)
(ql:quickload '(cl-json-ld))

;;;; Configuration -----------------------------------------------
(defparameter *ssb-server* "127.0.0.1:9092")
(defparameter *ssb-source-topic* "ssb-source-topic")

;;; Define command line options. We do this with
;;; `define-opts' macro.

(opts:define-opts
  (:name :help
         :description "print this help text"
         :short #\h
         :long "help")
  (:name :iter
         :description "the program will generate ITER messages"
         :required t
         :short #\i
         :long "iter"
         :arg-parser #'parse-integer
         :meta-var "ITER"))


;;;; Functionality ============================================================

(defun current-date-time-string ()
  "Returns current date-time as a string in ISO 8601 format"
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@A" yr mon day hr min sec)))

;;; Core functions ============================================================

(defun build-message ( index )
  "Arbitrary (key value) JSON message generator"
  (let (
        (k (cl-json-ld:jsd-to-string (cl-json-ld:jsd-make "foo" index
                                    "bar" index)))
        (v (cl-json-ld:jsd-to-string (cl-json-ld:jsd-make "vfoo" index
                                    "vbar" index)))
       )
    (list :key k :value v)
  )
)

(defun build-nquad-message (index)
  (let (
  	(nquad-string (format NIL
  		"<http://example.org/resource/~d> <http://example.org/resourceLogisticId> \"~d\" <http://example.org/graph/temporary/>.
  		 <http://example.org/resource/~d> <http://example.org/resourceType> <http://example.org/SomeResourceType> <http://example.org/graph/temporary/>.
         <http://example.org/resource/~d> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Resource> <http://example.org/graph/temporary/>.
        " index index index index index))
        ; <http://example.org/resource/~d> <http://www.w3.org/ns/prov#generatedAtTime> \"~A\" <http://example.org/graph/temporary/>.
    )
    (write nquad-string)))

(defun build-message-jld ( index )
  "Arbitrary (key value) JSON-LD message generator"
  (let (
        (k (cl-json-ld:jsd-to-string (cl-json-ld:jsd-make "type" "JSON-LD")))
        (v (cl-json-ld:jsd-to-string (cl-json-ld:from-rdf (build-nquad-message index))))
       )
    (list :key k :value v)
  )
)

(defun produce ( iter )
  "Send 'iter' messages to kafka topic"

  (let ((producer (make-instance
                  'kf:producer
                  :conf `("bootstrap.servers" ,*ssb-server*
                         "enable.idempotence" "true")
                         :serde #'babel:string-to-octets)))

  (dotimes (n iter)
           (setq message (build-message-jld n)) ; generate message content
           (kf:send producer *ssb-source-topic* (getf message :value) :key (getf message :key)) ; send message to topic
           (format t "~% Key = ~A  Value = ~A" (getf message :key) (getf message :value)) ; send message to stdout
           ;; note there's no embedded 'wait' function in LISP, hence this will produce a burst of N messages
           )
  (kf:flush producer))
)

;;; Entry point

;;; OK, since command line options can be malformed we should use a handy
;;; Common Lisp feature: restarts. Unix-opts gives us all we need to do so.
;;; Here we define a function that will print a warning and ignore
;;; unknown-option. Several restarts (behaviors) are available for every
;;; exception that Unix-opts can throw. See documentation for `get-opts'
;;; function for more information.

(defun unknown-option (condition)
  (format t "warning: ~s option is unknown!~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

(defmacro when-option ((options opt) &body body)
  `(let ((it (getf ,options ,opt)))
     (when it
       ,@body)))

(multiple-value-bind (options free-args)
    (handler-case
        (handler-bind ((opts:unknown-option #'unknown-option))
          (opts:get-opts))
      (opts:missing-arg (condition)
        (format t "fatal: option ~s needs an argument!~%"
                (opts:option condition)))
      (opts:arg-parser-failed (condition)
        (format t "fatal: cannot parse ~s as argument of ~s~%"
                (opts:raw-arg condition)
                (opts:option condition)))
      (opts:missing-required-option (con)
        (format t "fatal: ~a~%" con)
        (opts:exit 1)))
  ;; Here all options are checked independently, it's trivial to code any
  ;; logic to process them.
  (when-option (options :help)
    (opts:describe
     :prefix "example—program to demonstrate unix-opts library"
     :suffix "so that's how it works…"
     :usage-of "producer.sh"
     :args     "[FREE-ARGS]"))
  (when-option (options :iter)
    (format t "I see you've supplied the iter option, you want ~a messages!~%" it)
    (setq iter it))
  (produce iter))

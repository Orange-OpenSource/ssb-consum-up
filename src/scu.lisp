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
;;;; This file is part of the SSB-CONSUM-UP project: SSB-CONSUM-UP (SCU) program

;;;; File template source: https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/

;;; cl-json-ld: first git clone the project from https://github.com/RDProjekt/cl-json-ld
;;; then adjust and evaluate the two following line
(push #p"./lib/cl-json-ld/" asdf:*central-registry*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload '(
                  :adopt            ; Parsing UNIX-style command line arguments, https://docs.stevelosh.com/adopt/usage/
                  :with-user-abort
                  alexandria
                  cl-rdkafka babel  ; Consumer transaction, https://github.com/SahilKang/cl-rdkafka
                  :drakma           ; HTTP package, https://edicl.github.io/drakma/
                  cl-json-ld        ; https://github.com/RDProjekt/cl-json-ld
                  ) :silent t))

(defpackage :scu
  (:use :cl alexandria)
  (:export :toplevel *ui*))

(in-package :scu)

;;;; Configuration -----------------------------------------------
(defparameter *ssb-server* "127.0.0.1:9092")
(defparameter *ssb-group-id* "some-group-id")
(defparameter *ssb-auto-commit* "false")
(defparameter *ssb-auto-offset-reset* "earliest")
(defparameter *ssb-source-topic* "ssb-source-topic")
(defparameter *ssb-consumer-batch-size* 15)
(defparameter *ssb-consumer-wait-period* 5000)

(defparameter *ep-url* "http://localhost:8890/sparql")
(defparameter *ep-default-action* "INSERT")
(defparameter *ep-default-graph* NIL)

(defparameter *tr-named-graph* NIL)

;;; Log headers to the REPL output stream
(setf drakma:*header-stream* *standard-output*)

;;;; RDF constants
;;; Source: https://github.com/RDProjekt/cl-json-ld/blob/master/src/jsonld.lisp
(alexandria:define-constant +rdf+ "http://www.w3.org/1999/02/22-rdf-syntax-ns#" :test #'string=)
(alexandria:define-constant +rdf-type+ (concatenate 'string +rdf+ "type") :test #'string=)

;;;; Errors ------------------------------------------------------
(define-condition user-error (error) ())

(define-condition missing-foo (user-error) ()
  (:report "A foo is required, but none was supplied."))

;;;; Functionality ===============================================
(defun foo (string)
  (format t "Had ~S ~%" string))

(defun current-date-time-string ()
  "Returns current date-time as a string in ISO 8601 format"
  (multiple-value-bind (sec min hr day mon yr dow dst-p tz)
                       (get-decoded-time)
    (declare (ignore dow dst-p tz))
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,1,0,'0@A:~2,1,0,'0@A:~2,1,0,'0@A" yr mon day hr min sec)))

;;;; Functionality: SPARQL Update implementation -----------------

(defun make-message (query-type target-graph payload)
  "Build a SPARQL Update message (see https://www.w3.org/TR/sparql11-update/)"
  ; TODO: make DELETE request ok for EP syntax
  ;; ( WITH IRIref  )?
  ;; ( ( DeleteClause InsertClause? ) | InsertClause )
  ;; ( USING ( NAMED )? IRIref )*
  ;; WHERE GroupGraphPattern
  ;; Example with named graph: (make-message "INSERT DATA" "<http://example.org/graph/ExampleGraph>" "<http://example.org/ex1> a <http://example.org/ns#ExampleRecord> .")
  ;; Example w/ named graph: (make-message "INSERT DATA" NIL "<http://example.org/ex1> a <http://example.org/ns#ExampleRecord> .")
  (if (null target-graph)
    ;; No target-graph declared
    (format nil "~a { ~a  }" query-type payload)
    ;; Using a target-graph
    (format nil "~a { GRAPH ~a { ~a } }" query-type target-graph payload)
  )
)

(defun send-su-query (&key endpoint query-type target-graph payload)
  "Send a SPARQL Update query to some endpoint."
  ;; Example: (send-su-query :query-type "INSERT" :target-graph "<http://example.org/graph/ExampleGraph>" :payload "<http://example.org/ex1> a <http://example.org/ns#ExampleRecord> .")
  ;; Note that SPARQL_SPONGE and SPARQL_UPDATE role (or alike) might be required for the implicit SPARQL user
  ;; Note that some SPARQL end-point might require a target graph
  ;; Checking the transaction can be carried-out with wireshark, 'tcp port http or https or 8890' capture filter
  ;;TODO: align example with function definition

  ;; Logging
  (format t "[INFO][~A] (>>>) SEND-SU-QUERY: ~s ~s ~s ~s ~%" (current-date-time-string) endpoint query-type target-graph payload)

  ;; Send query
  (drakma:http-request
      endpoint
      :method :get
      :parameters (list (cons '"query" (make-message query-type target-graph payload)))
  ))

;;;; Functionality: JSON-LD handler implementation ---------------

(defun get-graph-content (c-jld)
  "Return a plist from a cl-json-ld hash table object describing a graph"
  (alexandria:hash-table-plist (caadr (alexandria:hash-table-plist (car c-jld)))))

(defun getf-string-equal (plist indicator &optional printcontent)
  "A helper function for gettting a property from a plist, and print list content if printcontent set to T."
  (loop
    for (i v) on plist by #'cddr
    if (eq printcontent T) do (format t "i=~s eq=~s v=~s ~%" i (string-equal i indicator) v)
    when (string-equal i indicator)
    return v))

(defun get-graph-name (c-jld &optional brackets)
  "Return string from a cl-json-ld hash table object with the target graph name.
  When 'brackets' set to T, add angle brackets to the output string"
  (if brackets
  	(format NIL "<~a>" (getf-string-equal (alexandria:hash-table-plist (car c-jld)) "@id"))  ; brackets = T
	(getf-string-equal (alexandria:hash-table-plist (car c-jld)) "@id")  ; brackets = NIL
))

(defun get-subject (plist)
  "Get the subject of some graph content"
  (getf-string-equal plist "@id"))

(defun get-class (plist)
  "Get the class of some graph content"
  (getf-string-equal plist "@type"))

(defun get-po-list (plist)
  "Get the list of (p,o) of some graph content, but the class predicate."
  (loop :for (i v) on plist by #'cddr
        :if (and (string-not-equal i "@id") (string-not-equal i "@type"))
          :collect (cons i (alexandria:hash-table-values (car v))) :into out-list
        :end
    :finally (return out-list))
  )

(defun get-pc-list (payload)
  "Get the list of (p,class) of some graph content."
  (loop :for c in (getf-string-equal payload "@type")
        :collect (list +rdf-type+ c) :into out-list
        :finally (return out-list))
  )

(defun has-http-prefix (item)
  "Check that item is alike an URI by looking for an http:// or https:// prefix."
  (if (> (length item) 8)
    (let ((prefixcandidate (subseq item 0 7)))
      (cond
        ((string-equal "http://" prefixcandidate) t)
        ((string-equal "https:/" prefixcandidate) t)
        ))))

(defun make-ntriples (subject polist)
  "Build N-Triple(s) as a string"
  ;; See https://stackoverflow.com/questions/5457346/lisp-function-to-concatenate-a-list-of-strings
  (if (listp polist)
	  (let ((result ""))
		(dolist (item polist)
				(let ((p (car item)) (o (cadr item)))
				  (if (has-http-prefix o)
					(setq result (concatenate 'string result (format nil "<~a> <~a> <~a> . " subject p o))) ; T
					(setq result (concatenate 'string result (format nil "<~a> <~a> ~S . " subject p o))) ; NIL
				  )))
		result)))


;;;; Functionality: Consumer implementation ----------------------

(defun get-next-batch (consumer max-batch-size)
	; TODO: function documentation
  (loop
    with batch = (make-array 0 :adjustable t :fill-pointer 0)

    repeat max-batch-size
    for message = (kf:poll consumer *ssb-consumer-wait-period*)
    while message
    do (vector-push-extend message batch)

    finally (return batch)))

(defun process-message (&key message ep-url ep-default-graph ep-default-action tr-named-graph)
  "Parse the incoming Kafka message and send a SPARQL Update query correspondingly"
  (let (
        (display-value (format t "[INFO][~A] (<<<) PROCESS-MESSAGE:RECEIVED = ~A~%" (current-date-time-string) (kf:value message)))
        (pl-payload (get-graph-content (cl-json-ld:jsd-read (kf:value message))))
        (target-graph (if (eql T tr-named-graph) ; TODO: handle empty named graph case from incoming message
        	(get-graph-name (cl-json-ld:jsd-read (kf:value message)) T)  ; tr-named-graph = T   : use incoming named-graph
        	ep-default-graph))                                           ; tr-named-graph = NIL : use default named-graph
       )

    ;; TODO: Based on message syntax, call the right SPARQL update generator, or drop message
    (send-su-query
    	:endpoint ep-url
    	:query-type ep-default-action  ; TODO: make use of incoming action if available and authorized by options
		:target-graph target-graph
		:payload (make-ntriples (get-subject pl-payload) (append (get-pc-list pl-payload) (get-po-list pl-payload)))
    )))

(defun process-batch (consumer ep-url ep-default-graph ep-default-action tr-named-graph)
  "Get a set of messages from the Kafka consumer and, for each, call the process-message function"
  (let ((messages (get-next-batch consumer *ssb-consumer-batch-size*)))
    (map nil
         (lambda (message)
           (process-message
				:message message
				:ep-url ep-url
				:ep-default-graph ep-default-graph
				:ep-default-action ep-default-action
				:tr-named-graph tr-named-graph))
         messages)
	(kf:commit consumer)
    )
  )

;;;; Run =========================================================
(defun run (&key ssb-server ssb-source-topic ep-url ep-default-graph ep-default-action tr-named-graph)
  "Takes some arguments (as strings) and performs the main work of the program:
    - instanciate a kafka consumer,
    - subscribe to a source topic,
    - start a processing loop.
    Interactive call example:
    (cl-scu:run :ssb-server '*ssb-server* :ssb-source-topic '*ssb-source-topic*)
  "
  (let (
	;;; Instanciate the Kafka Consumer
  	(consumer (make-instance
                   'kf:consumer
                   :conf `("bootstrap.servers" ,ssb-server
                           "group.id" ,*ssb-group-id*  ; TODO: get value from options
                           "enable.auto.commit" ,*ssb-auto-commit*  ; TODO: get value from options
                           "auto.offset.reset" ,*ssb-auto-offset-reset*)  ; TODO: get value from options
                   :serde #'babel:octets-to-string))
	;;; Complementary variables for looping
	(IsDone NIL)
	(n 0))
	;;; Subscribe to the Kafka source topic
    (kf:subscribe consumer ssb-source-topic)  ; TODO: handle "1/1 brokers are down", notably if the following processing loop have to start
	;;; Process incoming messages within an infinite loop
    (loop
     (when (eql T IsDone) (return))
      (handler-case
      	(progn (format t "[DEBUG][~A] RUN:LOOP~%" (current-date-time-string))
         	  (process-batch consumer ep-url ep-default-graph ep-default-action tr-named-graph))
        (kf:fatal-error (c) (error c))))))

;;;; User Interface options --------------------------------------

(defparameter *option-help*
  (adopt:make-option 'help
    :help "Display help and exit."
    :long "help"
    :short #\h
    :reduce (constantly t)))

(adopt:defparameters (*option-debug* *option-no-debug*)
  (adopt:make-boolean-options 'debug
    :long "debug"
    :short #\d
    :help "Enable the Lisp debugger."
    :help-no "Disable the Lisp debugger (the default)."))

(defparameter *option-ssb-server*
  (adopt:make-option 'ssb-server
    :parameter "IP:PORT:STRING"
    :help (format nil "The IP:PORT of the Semantic Service Bus (Kafka instance) to consume messages from (default: ~S)." *ssb-server*)
    :long "ssb-server"
    :short #\s
    :initial-value *ssb-server*
    :reduce #'adopt:last))

(defparameter *option-ssb-source-topic*
  (adopt:make-option 'ssb-source-topic
    :parameter "TopicName:STRING"
    :help (format nil "The topic name of the Semantic Service Bus (Kafka instance) to consume messages from (default: ~S)." *ssb-source-topic*)
    :long "ssb-source-topic"
    :short #\t
    :initial-value *ssb-source-topic*
    :reduce #'adopt:last))

(defparameter *option-ep-url*
  (adopt:make-option 'ep-url
    :parameter "URL:STRING"
    :help (format nil "The URL of the SPARQL End Point (graph-store instance) to send SPARQL Update queries to (default: ~S)." *ep-url*)
    :long "ep-url"
    :short #\e
    :initial-value *ep-url*
    :reduce #'adopt:last))

(defparameter *option-ep-default-graph*
  (adopt:make-option 'ep-default-graph
    :parameter "GraphIRI:STRING"
    :help (format nil "The default data set name (Graph IRI) of the SPARQL End Point to use in SPARQL Update queries when no named graph is present in received data from the SSB (default: ~S)." *ep-default-graph*)
    :long "ep-default-graph"
    :short #\g
    :initial-value *ep-default-graph*
    :reduce #'adopt:last))

(defparameter *option-ep-default-action*
  (adopt:make-option 'ep-default-action
    :parameter "SparqlUpdateAction:STRING"
    :help (format nil "The default SPARQL Update action to use in SPARQL Update queries towards the SPARQL End Point when no action is present in received data from the SSB (default: ~S)." *ep-default-action*)
    :long "ep-default-action"
    :short #\a
    :initial-value *ep-default-action*
    :reduce #'adopt:last))

(defparameter *option-tr-named-graph*
  (adopt:make-option 'tr-named-graph
    :help "Use incoming message's named graph for the SPARQL Update query."
    :long "tr-named-graph"
    :initial-value *tr-named-graph*
    :reduce (constantly t)))

; TODO: add "prioritize incoming config over local config" option
; TODO: add "drop non conforming incoming messages" option (?)

;;;; User Interface help -----------------------------------------

(adopt:define-string *help-text*
  "The SSB-CONSUM-UP (SCU) program consumes JSON-LD data from a Kafka topic and ~
   forward the payload to a graph store through SPARQL Update queries.~@
   ~@
   Press Ctrl-C for stopping the SCU instance.")

(adopt:define-string *extra-manual-text*
  "Graph data is processed from the 'value' part of the incoming Kafka message.~
  The 'key' part of the Kafka message is left for future use.")

(defparameter *examples*
  '(("Start SSB-CONSUM-UP with the default parameters:"
     . "scu")
	("Send incoming triples to a specific named graph:"
          . "scu -g \"<http://example.org/graph/ssb-consum-up>\"")
    ))

;;;; User Interface ----------------------------------------------
(defmacro exit-on-ctrl-c (&body body)
  `(handler-case (with-user-abort:with-user-abort (progn ,@body))
     (with-user-abort:user-abort () (sb-ext:exit :code 130))))

(defparameter *ui*
  (adopt:make-interface
    :name "scu"
    :usage "[OPTIONS]"
    :summary "consume JSON-LD data from a Kafka topic and forward the payload to a graph store through SPARQL Update queries"
    :help *help-text*
    :manual (format nil "~A~2%~A" *help-text* *extra-manual-text*)
    :examples *examples*
    :contents (list
                *option-help*
                *option-debug*
                *option-no-debug*
                (adopt:make-group 'ssb-options
                                  :title "SSB Options"
                                  :options (list *option-ssb-server*
                                                 *option-ssb-source-topic*))
                (adopt:make-group 'ep-options
                                  :title "SPARQL End Point Options"
                                  :options (list *option-ep-url*
                                                 *option-ep-default-graph*
                                                 *option-ep-default-action*))
                (adopt:make-group 'tr-options
                                  :title "Message Transform Options"
                                  :options (list *option-tr-named-graph*))
                )
    ))

(defun toplevel ()
  "Program entrypoint"
  (sb-ext:disable-debugger)
  (exit-on-ctrl-c
    (multiple-value-bind (arguments options) (adopt:parse-options-or-exit *ui*)
		(when (gethash 'debug options)
			(sb-ext:enable-debugger))
		(when (gethash 'ssb-server options)  ; TODO: move INIT messages in the RUN function
			(format t "[DEBUG][~A] INIT:ssb-server = ~A ~%" (current-date-time-string) (gethash 'ssb-server options)))
		(when (gethash 'ssb-source-topic options)
			(format t "[DEBUG][~A] INIT:ssb-source-topic = ~A ~%" (current-date-time-string) (gethash 'ssb-source-topic options)))
		(when (gethash 'ep-url options)
			(format t "[DEBUG][~A] INIT:ep-url = ~A ~%" (current-date-time-string) (gethash 'ep-url options)))
		(when (gethash 'ep-default-graph options)
			(format t "[DEBUG][~A] INIT:ep-default-graph = ~A ~%" (current-date-time-string) (gethash 'ep-default-graph options)))
		(when (gethash 'ep-default-action options)
			(format t "[DEBUG][~A] INIT:ep-default-action = ~A ~%" (current-date-time-string) (gethash 'ep-default-action options)))
		(when (gethash 'tr-named-graph options)
			(format t "[DEBUG][~A] INIT:tr-named-graph = ~A ~%" (current-date-time-string) (gethash 'tr-named-graph options)))

    	(handler-case
        	(cond
	          ((gethash 'help options) (adopt:print-help-and-exit *ui*))
	          (t (run
	          		:ssb-server (gethash 'ssb-server options)
	          		:ssb-source-topic (gethash 'ssb-source-topic options)
	          		:ep-url (gethash 'ep-url options)
	          		:ep-default-graph (gethash 'ep-default-graph options)
	          		:ep-default-action (gethash 'ep-default-action options)
	          		:tr-named-graph (gethash 'tr-named-graph options)
	          	))
    	    )
        	(user-error (e) (adopt:print-error-and-exit e)))
        )))

#!/usr/bin/env bash
# Builds the man pages using Adopt's built-in man page generation.
# Source: https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/
set -euo pipefail

LISP=$1
NAME=$(basename "$LISP" .lisp)
OUT="$NAME.1"
shift

sbcl --load "$LISP" \
     --eval "(with-open-file (f \"$OUT\" :direction :output :if-exists :supersede)
               (adopt:print-manual $NAME:*ui* :stream f))" \
     --quit

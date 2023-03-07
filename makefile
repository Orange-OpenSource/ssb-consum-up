# Copyright (c) 2022-2023 Orange. All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#     1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
#     2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
#     3. All advertising materials mentioning features or use of this software must display the following acknowledgement:
#     This product includes software developed by Orange.
#     4. Neither the name of Orange nor the names of its contributors may be used to endorse or promote products derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY Orange "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL Orange BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# =============================================================================

## makefile for the SSB-CONSUM-UP project
##
## Use `make scu` for building the SSB-CONSUM-UP (scu) binary and man page
##

# === General ==================================================================

# Loading (optional) environment variables from file.
-include ./.env

CURL_DIRECT := curl
CURL_PROXY := curl -x $(PROXY_URL)

help:	## Show this help.
	@fgrep -h "## " $(MAKEFILE_LIST) | fgrep -v fgrep | sed 's/\\$$//' | sed 's/## //'

# === BUILD ====================================================================

SRC_DIRS := ./src
BIN_DIR := ./bin
MAN_DIR := ./man

# Find all the LISP files we want to compile
SRCS := $(shell find $(SRC_DIRS) -name '*.lisp')
NAMES := $(SRCS:.lisp=)

.PHONY: all clean $(NAMES)

all: $(NAMES)

$(NAMES): %: $(BIN_DIR)/% $(MAN_DIR)/man1/%.1

$(BIN_DIR)/%: %.lisp build-binary.sh makefile
	@echo "\033[35m > Start BUILD bin  \033[0m"
	mkdir -p bin
	./build-binary.sh $<
	mv $(@F) $(BIN_DIR)/
	@echo "\033[35m > Done  \033[0m"

$(MAN_DIR)/man1/%.1: %.lisp build-manual.sh makefile
	@echo "\033[35m > Start BUILD man  \033[0m"
	mkdir -p $(MAN_DIR)/man1
	./build-manual.sh $<
	mv $(@F) $(MAN_DIR)/man1/
	@echo "\033[35m > Done  \033[0m"

clean:  ## BUILD - remove bin and man directories
	@echo "\033[35m > Start CLEAN  \033[0m"
	rm -rf $(BIN_DIR)/*
	rm -rf $(MAN_DIR)/*
	@echo "\033[35m > Done  \033[0m"

install-dev-tools:	## Install requirements and development tools
	@echo "\033[35m > Install Steel Bank Common Lisp  \033[0m (see http://www.sbcl.org/)"
	@sudo apt-get install sbcl

	@echo "\033[35m > Install the Quicklisp package manager \033[0m (see https://www.quicklisp.org/beta/)"
	@echo "\033[35m >> PROXY_URL = $(PROXY_URL) \033[0m"
ifndef PROXY_URL
	@echo "\033[35m >> Direct connexion \033[0m"
	$(CURL_DIRECT) -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
endif
ifdef PROXY_URL
	@echo "\033[35m >> Using PROXY_URL \033[0m"
	$(CURL_PROXY) -o /tmp/ql.lisp http://beta.quicklisp.org/quicklisp.lisp
endif
	sbcl --no-sysinit --no-userinit --load /tmp/ql.lisp \
		   --eval '(quicklisp-quickstart:install :path "~/.quicklisp")' \
		   --eval '(ql:add-to-init-file)' \
		   --quit

	@echo "\033[35m > Install docker-compose  \033[0m"
	sudo apt install docker-compose

	@echo "\033[35m > Done  \033[0m"


# === DEMO =====================================================================

start-kafka:	## DEMO - start a Docker bitnami kafka instance on localhost
	@echo "\033[35m > Start a Docker bitnami kafka instance on localhost with docker-compose \033[0m"
	@docker-compose -p ssb-consum-up -f ./demo/docker-compose.yml up -d
	@echo "\033[35m > Done  \033[0m"

stop-kafka:	## DEMO - stop the Kafka instance on localhost
	@echo "\033[35m > Stop the Kafka instance on localhost  \033[0m"
	docker-compose -p ssb-consum-up -f ./demo/docker-compose.yml down
	@echo "\033[35m > Done  \033[0m"

start-virtdb:  ## DEMO - start a Virtuoso container instance as a service and map 2 local dir for data access
	@echo "\033[35m > Start a Virtuoso container instance as a service \033[0m"
	@mkdir -p virtdb
	@docker run \
	     --name virtdb \
	     --interactive \
	     --rm \
	     -d \
	     --tty \
	     --env DBA_PASSWORD=mysecret \
	     --publish 1111:1111 \
	     --publish  8890:8890 \
	     --volume `pwd`/virtdb:/database \
	     --volume `pwd`/demo:/mnt \
	     openlink/virtuoso-opensource-7

	@echo "\033[35m > Wait for DB to start before continuing \033[0m"
	sleep 3s

	@echo "\033[35m > Send configuration file \033[0m"
	@docker exec \
	  --interactive \
	  virtdb isql localhost dba mysecret /mnt/virtdb-config.sql || true

	@echo "\033[35m > Done  \033[0m"

stop-virtdb:	## DEMO - stop the Virtuoso instance on localhost
	@echo "\033[35m > Stop the Virtuoso instance on localhost  \033[0m"
	@docker container stop virtdb
	@echo "\033[35m > Done  \033[0m"

start-producer:	## DEMO - call the producer-iter-json.lisp script
	@echo "\033[35m > Call the producer script \033[0m"
	@sbcl --noinform --non-interactive --quit --load ./demo/producer-iter-json.lisp --end-toplevel-options "--iter=80"
	@echo "\033[35m > Done  \033[0m"

get-demo-data:  ## DEMO - fetch demo data from the virtdb instance
	@echo "\033[35m > Fetch demo data from the virtdb instance based on SCU's default parameters \033[0m"
	@curl --digest --user dba:mysecret --verbose --url "http://localhost:8890/sparql-graph-crud?graph-uri=http://example.org/graph/ssb-consum-up"
	@echo "\033[35m > Done  \033[0m"

# === TEST ====================================================================

test: start-scu-script-help

start-docker-sbcl:	## TESTING - start a clfoundation/sbcl instance for interactive testing
	@echo "\033[35m > Start a clfoundation/sbcl instance. See /source for project code \033[0m"
	@docker run --rm -it -v `pwd`:/source clfoundation/sbcl:latest bash
	@echo "\033[35m > Done  \033[0m"

# === RUN ======================================================================

start-scu-script:	## RUN - call the scu.lisp script with default parameters and <http://example.org/graph/ssb-consum-up> target graph
	@echo "\033[35m > Call the scu.lisp script \033[0m"
	@sbcl --quit --non-interactive --load ./src/scu.lisp \
	  --eval "(scu:toplevel)" \
	  --end-toplevel-options "-g <http://example.org/graph/ssb-consum-up>"
	@echo "\033[35m > Done  \033[0m"

start-scu-script-help:	## RUN - call the scu.lisp script with --help option
	@echo "\033[35m > Call the scu.lisp script \033[0m"
	@sbcl --quit --non-interactive --load ./src/scu.lisp \
	  --eval "(scu:toplevel)" \
	  --end-toplevel-options "--help"
	@echo "\033[35m > Done  \033[0m"

start-scu-binary:	## RUN - call the scu binary with default parameters and <http://example.org/graph/ssb-consum-up> target graph
	@echo "\033[35m > Call the scu binary \033[0m"
	@bin/scu -g "<http://example.org/graph/ssb-consum-up>"
	@echo "\033[35m > Done  \033[0m"

start-scu-binary-help:	## RUN - call the scu binary with --help option
	@echo "\033[35m > Call the scu binary \033[0m"
	@bin/scu --help
	@echo "\033[35m > Done  \033[0m"

# ssb-consum-up (scu)

A Semantic Service Bus (SSB) consumer to SPARQL Update.

![scu_principle](docs/diagrams/scu_principle.png)

The *ssb-consum-up* (*scu* thereafter) solution consumes Kafka messages from some topic where the *<k,v>* message structure contains RDF data in JSON-LD syntax.

* *v (value)*: graph data payload, whether part or not of a named graph (i.e. derived from N-Quads or Trig data *vs* derived from triples )
* *k (key)*: optional metadata for complementary downstream SPARQL Update behavior configuration, such as providing the SPARQL Update action to use, a payload provenance, etc.

Processed semantic messages trigger SPARQL Update queries to some downstream SPARQL end point, thus enabling tasks such as inserting streams of RDF data.

Additional upstream processing (e.g. message filtering at the Kafka engine level) or downstream architectures (e.g. 1 topic to multiple end points) can be tought of with the SCU solution, although this discussion falls out of the scope of this code repository.

## Quick-start

* Clone project
* Install the Steel Bank Common Lisp ([SBCL](http://www.sbcl.org/)) compiler and the [Quicklisp](https://www.quicklisp.org/beta/) package manager: `make install-dev-tools` (or see related docs)
* Start the Kafka message bus: `make start-kafka` (rely on the [bitnami/kafka](https://hub.docker.com/r/bitnami/kafka) Docker image)
* Start the Virtuoso database: `make start-virtdb` (rely on the [openlink/virtuoso-opensource-7](https://hub.docker.com/r/openlink/virtuoso-opensource-7/) Docker image)
* Start the SCU script with default values: `make start-scu-script`
* Start the demo producer script: `make start-producer`
* Observe the SCU logs for data consume/update notifications
* Get the inserted demo data from the Virtuoso database: `make get-demo-data`
* Stop the SCU: `Ctrl-C`
* Stop the demo: `make stop-kafka` + `make stop-virtdb`

## Usage

### Build and run binary from CLI

Use the following shell commands for working with the SSB-CONSUM-UP **binary release**:

```shell
# Build (makefile)
make all

# Run (without options = default options)
bin/scu

# Run (show help)
bin/scu --help

# Run (define target named graph)
bin/scu -g "<http://example.org/graph/ssb-consum-up>"

# Run (show man page)
man man/man1/scu.1
```

### Run script from CLI

Use the following shell commands for working with the SSB-CONSUM-UP **LISP script**:

```shell
# Run (without options)
sbcl --quit --non-interactive --load ./src/scu.lisp --eval "(scu:toplevel)"

# Run (show help)
sbcl --quit --non-interactive --load ./src/scu.lisp --eval "(scu:toplevel)" --end-toplevel-options "--help"

# Run (define target named graph)
sbcl --quit --non-interactive --load ./src/scu.lisp --eval "(scu:toplevel)" --end-toplevel-options "-g <http://example.org/graph/ssb-consum-up>"
```

### <k,v> message structure

The SSB-CONSUM-UP handles incoming Kafka messages in JSON-LD syntax of the following kind:

* Key (optional) =
  ```json
  {"type":"JSON-LD"}
  ```
* Value =
  ```json
  [{
    "@id":"http://example.org/graph/temporary/",
    "@graph":[{
      "@id":"http://example.org/resource/74",
      "http://example.org/resourceLogisticId":[{
        "@value":"74"}],
      "http://example.org/resourceType":[{
        "@id":"http://example.org/vocab/SomeResourceType"}],
      "@type":["http://example.org/Resource"]}
    ]
  }]
  ```

The *value* part is typically derived from a set of N-Quads assertions related to the same subject.

## Citation

If you use this software in a scientific publication, please cite:

> Lionel Tailhardat, Yoan Chabot, and Raphaël Troncy. 2023.
> Designing NORIA: a Knowledge Graph-based Platform for Anomaly Detection and Incident Management in ICT Systems.
> In 4th International Workshop on Knowledge Graph Construction (KGC).

BibTex format:

```bibtex
@inproceedings{noria-di-2023,
  title = {{Designing NORIA: a Knowledge Graph-based Platform for Anomaly Detection and Incident Management in ICT Systems}},
  booktitle = {{4th International Workshop on Knowledge Graph Construction (KGC)}},
  author = {{Lionel Tailhardat} and {Yoan Chabot} and {Rapha\"el Troncy}},
  year = {2023}
}
```

## Copyright

Copyright (c) 2022-2023, Orange / EURECOM. All rights reserved.

## License

[BSD-4-Clause](LICENSE.txt).

## Maintainer

* [Lionel TAILHARDAT](mailto:lionel.tailhardat@orange.com)
* [Yoan CHABOT](mailto:yoan.chabot@orange.com)
* [Raphaël TRONCY](mailto:raphael.troncy@eurecom.fr)

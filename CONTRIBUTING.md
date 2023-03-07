# Contributing to ssb-consum-up (scu)

We would love for you to contribute to **ssb-consum-up** and help make it even better than it is today!

Please contribute to this repository if any of the following is true:

- You have expertise in *Semantic Web*, *Knowledge Graph Construction* or *RDF Stream Reasoning*,
- You have expertise in *message passing*, *functional programming*, *network protocols* or *reasoning systems*,
- You want to contribute to the development of *Semantic Web technologies* and want to help lower the burden of managing *complex technical systems*.

## How to contribute

Prerequisites:

- Knowledge of [Semantic Web](https://en.wikipedia.org/wiki/Semantic_Web) technologies, notably:
    - [JSON-LD](https://json-ld.org/),
    - [Terse RDF Triple Language (Turtle)](https://en.wikipedia.org/wiki/Turtle_(syntax)),
    - [SPARQL Protocol and RDF Query Language](https://en.wikipedia.org/wiki/SPARQL) and [SPARQL Update](https://www.w3.org/TR/sparql11-update/) syntax.
- Knowledge of *stream processing / message broker* platforms, notably the [Apache Kafka](https://kafka.apache.org/) platform.
- Knowledge of *functional programming*, notably the [LISP](https://en.wikipedia.org/wiki/Lisp_(programming_language)) programming language.
- Familiarity with [pull requests](https://help.github.com/articles/using-pull-requests) and [issues](https://guides.github.com/features/issues/).
- Familiarity with [test design](https://en.wikipedia.org/wiki/Test_design) and techniques.
- Knowledge of [Markdown](https://help.github.com/articles/markdown-basics/) for editing `.md` documents.

In particular, we seek the following types of contributions:

- **Testing & Validating**: RDF streams have an intrinsic explainability characteristic thanks to annotated entities based on shared data models, which encourages to semantize data as soon as possible in the data processing tool chain.
  It is not clear however, from the data processing architecture, where stream reasoning modules should best stand, both in terms of performance,
  reasoning capabilities, distributed computing, and ease of management. Feedback on architectures and use-cases including the *ssb-consum-up* should help identify such decision boundaries and design templates (e.g. moving towards full Kappa architectures),
  particularly in terms of energy efficiency and network overhead.
- **Improving & Extending**: participate in making *ssb-consum-up* code smoother and more robust, for example by adding conditional SPARQL Update actions based on incoming metadata, enabling the processing of data payload with multiple target graph, enabling connection to SPARQL endpoints with authentication, adding multithreading or data buffering, etc.

When contributing, in the general case, please:

* *fork and create merge request* OR
* *raise an issue* into the project's space OR
* improve code based on in-code `TODO` notes or below features list.

See also the `makefile` for development and testing purposes.

Must have features:

* [X] Single graph from n-quad in JSON-LD message to SPARQL Update
* [ ] Multiple graphs from n-quad in JSON-LD message to SPARQL Update
* [ ] Single graph from triples in JSON-LD message to SPARQL Update
* [ ] Multiple graphs from triples in JSON-LD message to SPARQL Update
* [X] SPARQL Update *INSERT* as default action
* [ ] SPARQL Update action based on incoming Kafka key JSON-LD parameters
* [X] SPARQL Update to target named graph based on CLI parameters
* [X] SPARQL Update to target named graph based on incoming Kafka JSON-LD graph value
* [X] SPARQL Update to target named graph based on CLI parameters with override of the incoming Kafka JSON-LD graph value
* [ ] Automatic fall-back to CLI parametrized named graph and SPARQL UPDATE action when incoming Kafka key paramaters are missing
* [ ] Drop policy for non-compliant incoming messages

Nice to have features:
* [ ] LISP: reduce STYLE-WARNING (see [SBCL manual](http://www.sbcl.org/manual/#Style-Warnings))
* [ ] LISP: exception handling on no SPARQL endpoint
* [ ] LISP: light cl-json-ld load
* [ ] LISP: timeout feature for the infinite loop
* [ ] LISP: better logging
* [ ] SPARQL: handle end points that do not require the GRAPH clause for the default graph
* [ ] CI: test, build and publish binary
* [ ] Build generator


## Communication

GitHub issues are the primary way for communicating about specific proposed changes to this project.
Do not open issues for general support questions as we want to keep GitHub issues for bug reports and feature requests.

You may also contact the maintainers by e-mail for more specific purposes and questions.

In both contexts, please be kind and courteous. 
Language issues are often contentious and we'd like to keep discussion brief, civil and focused on what we're actually doing, not wandering off into too much imaginary stuff. 
Likewise any spamming, trolling, flaming, baiting or other attention-stealing behaviour is not welcome.

## Learn

Useful references for development ...

### Semantic Web references

* [SPARQL 1.1 Update](https://www.w3.org/TR/sparql11-update/)
* [RDF 1.1 N-Quads](https://www.w3.org/TR/n-quads/)

### LISP packages references

* [alexandria](https://quickref.common-lisp.net/alexandria.html) - a collection of portable public domain utilities
* [cl-json-ld](https://github.com/RDProjekt/cl-json-ld) - a library that implements JSON-LD functionality
* [librdkafka](https://github.com/edenhill/librdkafka) - the Apache Kafka C/C++ client library

### Additional references

* LISP: [stevelosh/building-binaries](https://stevelosh.com/blog/2021/03/small-common-lisp-cli-programs/#s5-building-binaries)
* LISP: [Adopt](https://docs.stevelosh.com/adopt/)
* LISP: [Lisp Cookbook - Testing](http://lispcookbook.github.io/cl-cookbook/testing.html)
* LISP: [Lisp Lang - Continuous Integration](https://lisp-lang.org/learn/continuous-integration)
* Makefile: [MakeFile tutorial](https://makefiletutorial.com/)
* GitLab: [GitLab CI - generic packages](https://docs.gitlab.com/ee/user/packages/generic_packages/index.html)

HyPTCTLChecker
==============

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](./LICENSE)

Prototype transpiler to handle hyperproperties in [IMITATOR](https://www.imitator.fr/).

Note: Some of the codes in this repository are forked from IMITATOR, which is also distributed under GPL v3.0.

Usage
-----

### Synopsis

```sh
dune exec HyPTCTLChecker [MODEL FILE] [PROPERTY FILE] [OUTPUT FILENAME]
```

### Example

Example files, including the ones used in the experiments in the paper, are available in the `examples` directory.

```sh
# The results are written to /tmp/results.imi and /tmp/results.imiprop
dune exec HyPTCTLChecker ./examples/deviation/clkgen.hyper-imi ./examples/deviation/deviation.hyper-imiprop /tmp/results
```

Installation
------------

HyPTCTLChecker is tested on Ubuntu (22.04) and macOS (14 Sonoma).

### Requirements

To build HyPTCTLChecker, the following are required. To use the resulting files, [IMITATOR](https://www.imitator.fr/) is also necessary. See the [official instructions](https://www.imitator.fr/download.html) for the installation of IMITATOR.

* OCaml compiler (>= 4.08)
* opam (>= 2.0)
* dune (>= 3.6)

Input File Formats
------------------

The model and property files for HyPTCTLChecker are based on the IMITATOR format with minor extensions to represent hyperproperties. The following summarizes the extensions. See, for example, `examples/deviation/clkgen.hyper-imi` and `examples/deviation/deviation.hyper-imiprop` for concrete examples.

- In the model file, each location is labeled with a set of atomic propositions. The atomic propositions are separated by commas. For example, `loc waiting: invariant True stop{c1}: label {WAITING1, SUBMITTED1}` represents a location `waiting` with atomic propositions `WAITING1` and `SUBMITTED1`.
- The property files consist of the following parts:
    - Definitions of the variables used in the extended properties such as `var COUNT_a_1: mod 4`.
    - Definitions of the extended predicates such as `let large_deviation = LAST_a_1 - LAST_a_2 < -param || LAST_a_1 - LAST_a_2 > param in`.
    - The flag `use global_time;` to indicate the name of the global clock variable if used.
    - The raw IMITATOR property enclosed by `<*` and `*>`.

License
-------

HyPTCTLChecker is distributed under GNU General Public License v3.0. See [LICENSE](./LICENSE) for more information. The codes forked from IMITATOR are also distributed under GPL v3.0. The license of each example file is specified in the files.

Reference
---------

* Hyper parametric timed CTL. Masaki Waga and Étienne André

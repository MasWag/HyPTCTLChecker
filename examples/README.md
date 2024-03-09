Examples
========

This directory contains the example models and properties, including the ones used in the experiments in the paper.

Correspondence of the models in the paper and in this directory
---------------------------------------------------------------

The following table shows the correspondence between the examples in this directory and the ones in the paper.

| Name in the Paper        | Path in this Directory                                  |
|--------------------------|---------------------------------------------------------|
| `ClkGen`                 | `deviation/clkgen-parametric.hyper-imi`                 |
| `Coffee` (`Opacity`)     | `opacity/coffee-common.hyper-imi`                       |
| `STAC1:n`                | `opacity/category1_not_vulnerable.hyper-imi`            |
| `STAC4:n`                | `opacity/category4_not_vulnerable.hyper-imi`            |
| `FIFO`                   | `unfairness/fifo_scheduler-parametric.hyper-imi`        |
| `Priority`               | `unfairness/priority_scheduler-parametric.hyper-imi`    |
| `R.R.`                   | `unfairness/round_robin_scheduler-parametric.hyper-imi` |
| `Coffee` (`RobOND`)      | `robust-OND/coffee-common.hyper-imi`                    |
| $\mathrm{WFAS}^1_0$      | `robust-OND/WFAS_BBLS15-instance1.hyper-imi`            |
| $\mathrm{WFAS}^2_0$      | `robust-OND/WFAS_BBLS15-instance2.hyper-imi`            |
| $\mathrm{WFAS}_1$        | `robust-OND/WFAS_BBLS15.hyper-imi`                      |
| $\mathrm{WFAS}_2$        | `robust-OND/WFAS_BBLS15-two_params.hyper-imi`           |
| `ATM`                    | `robust-OND/ATM.hyper-imi`                              |
| `ATM'`                   | `robust-OND/ATM-no-check.hyper-imi`                     |
| `Coffee` (`scalability`) | `scalability/coffee-common-non-parametric.hyper-imi`    |

Detail of the Models
--------------------

The following is the list of models and the changes from the original version

### Common changes

The common changes are as follows:

- If the original model is for IMITATOR 2, we updated its syntax for IMITATOR 3.
     - Notably, we use the native Boolean values, which is added in IMITATOR 3.
- If `abs_clock` and `abs_ptime` are defined, we removed them because they are automatically added by HyPTCTLChecker.
- If a parameter is used in the specification, we added a parameter `param` to the model.

### Deviation

The `deviation` directory contains the models such that their deviation property is verified. The deviation property is encoded in `deviation/deviation.hyper-imiprop`

- `deviation/clkgen-parametric.hyper-imi`
    - Our original model shown in the paper
- `deviation/clkgen.hyper-imi`
    - The original version: `deviation/clkgen-parametric.hyper-imi`
    - Not used in the paper

### Opacity

The `opacity` directory contains the models such that their opacity is verified. The opacity property is encoded in `opacity/opaque.hyper-imiprop`

The common changes are as follows:

- We removed `flag` variables because we do not use it to encode the opacity.
- Instead, we labeled the PRIVATE and GOAL locations to encode the opacity

The following is the list of models and the changes from the original version.

- `opacity/category1_not_vulnerable.hyper-imi`
    - The original version: https://www.imitator.fr/data/ATVA19/experiments/category1_not_vulnerable.imi
    - Modifications:
        - We removed the local parameters to encode symbolic data because only global parameters are supported in our formalism.
- `opacity/category4_not_vulnerable.hyper-imi`
    - The original version: https://www.imitator.fr/data/ATVA19/experiments/category4_not_vulnerable.imi
    - Modifications:
        - We added an auxiliary location because we need a private location rather than a private transition.
        - We modified the local parameters to global parameters.
- `opacity/coffee.hyper-imi`
    - Not used in the paper
    - The original version: https://www.imitator.fr/data/ATVA19/experiments/coffee-security-inst.imi
    - Modifications:
        - We parameterized the timing constraints
        - We added an auxiliary location because we need a private location rather than a private transition.
- `opacity/coffee-common.hyper-imi`
    - This model is based on `opacity/coffee.hyper-imi` but almost rewritten so that it also makes sense to verify Robust-OND.

### Unfairness

The `unfairness` directory contains the models of schedulers such that their unfairness property is verified. We have two versions of each model: one for parametric unfairness and the other for non-parametric unfairness. The unfairness property is encoded in `unfairness/unfair_scheduler-parametric.hyper-imiprop` and `unfairness/unfair_scheduler.hyper-imiprop`

- `unfairness/fifo_scheduler-parametric.hyper-imi`
    - Our original model encoding the FIFO scheduler
- `unfairness/fifo_scheduler.hyper-imi`
    - The original version: `unfairness/fifo_scheduler-parametric.hyper-imi`
    - Not used in the paper
    - Modifications:
        - This is for a non-parametric unfairness with acceptable duration = 5
- `unfairness/priority_scheduler-parametric.hyper-imi`
    - Our original model encoding the priority scheduler
- `unfairness/priority_scheduler.hyper-imi`
    - The original version: `unfairness/priority_scheduler-parametric.hyper-imi`
    - Not used in the paper
    - Modifications:
        - This is for a non-parametric unfairness with acceptable duration = 5
- `unfairness/round_robin_scheduler-parametric.hyper-imi`
    - Our original model encoding the round-robin scheduler
- `unfairness/round_robin_scheduler.hyper-imi`
    - The original version: `unfairness/round_robin_scheduler-parametric.hyper-imi`
    - Not used in the paper
    - Modifications:
        - This is for a non-parametric unfairness with acceptable duration = 5

### Robust-OND

The `robust-OND` directory contains the models such that their robust observational nondeterminism is verified.

- `robust-OND/ATM.hyper-imi`
    - The original version: https://github.com/imitator-model-checker/imitator/blob/master/benchmarks/ATM/fig1_DCLXZL18-fixed.imi
    - Modifications:
        - We removed the redundant clock variable.
        - We removed parameters by instantiating them with concrete values.
        - We split "Withdrawals" and "Check" locations into "ask_Withdrawals", "get_Withdrawals", and "ask_Check", "get_Check" locations.
            - We labeled these locations with "WithdrawIn", "WithdrawOut", "CheckIn", and "CheckOut" to encode the robust-OND property.
        - We modified the time out so that it is based on the time to stay in the login location, instead of the total time after the login.
- `robust-OND/ATM-no-check.hyper-imi`
    - The original version: `robust-OND/ATM.hyper-imi`
    - Modifications:
        - We removed the "Check" locations and the transitions related to the "Check" locations.
- `robust-OND/WFAS_BBLS15-two_params.hyper-imi`
    - The original version: https://github.com/imitator-model-checker/imitator/blob/master/benchmarks/WFAS/WFAS_BBLS15.imi
    - Modifications:
        - We constrained the parameters such that "p1 > 2" and "5 <= p2 & p2 <= 30".
        - We labeled the locations showing "WAKEUP1", "WAKEUP2", "DONE", and nothing to encode the robust-OND property.
- `robust-OND/WFAS_BBLS15.hyper-imi`
    - The original version: `robust-OND/WFAS_BBLS15-two_params.hyper-imi`
    - Modifications:
        - We instantiate so that "p1 = 5". The other parameters are not instantiated.
- `robust-OND/WFAS_BBLS15-instance1.hyper-imi`
    - The original version: `robust-OND/WFAS_BBLS15-two_params.hyper-imi`
    - Modifications:
        - We instantiate so that "p1 = 5" and "p2 = 19".
- `robust-OND/WFAS_BBLS15-instance2.hyper-imi`
    - The original version: `robust-OND/WFAS_BBLS15-two_params.hyper-imi`
    - Modifications:
        - We instantiate so that "p1 = 5" and "p2 = 9".
- `robust-OND/WFAS_BBLS15_det.hyper-imi`
    - Not used in the paper
    - The original version: https://github.com/imitator-model-checker/imitator/blob/master/benchmarks/WFAS/WFAS_BBLS15_det.imi
    - Modifications:
        - We labeled the locations showing "WAKEUP1", "WAKEUP2", "DONE", and nothing to encode the robust-OND property.
        - We instantiate so that "p1 = 5" and "p2 = 9".
- `robust-OND/coffee-common.hyper-imi`
    - Same as `opacity/coffee-common.hyper-imi`
- `robust-OND/coffee.hyper-imi`
    - Not used in the paper

### Scalability

The `scalability` directory contains the models to study the scalability of HyPTCTLChecker.perIMITATOR. To evaluate the near-maximum of path variables, we used a simplified variant of the coffee machine model. The properties we used are generated from `scalability/Makefile`.

- `scalability/coffee-common-non-parametric.hyper-imi`
    - The original version: `opacity/coffee-common.hyper-imi`
    - Modifications:
        - We instantiated so that "p1 = 1", "p2 = 5", and "p3 = 5".
- `scalability/coffee.hyper-imi`
    - Not used in the paper

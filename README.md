# An Algebraic Layered Architecture for Leader-Based BFT

Scala 3 implementation of the five layers of the MSc dissertation *An
Algebraic Layered Architecture for Leader-Based BFT* (Francisco Silveira and António Ravara
NOVA School of Science and Technology), instantiated for Tendermint under two
trust models:

- Θ_std: software signatures, N ≥ 3f + 1.
- Θ_tee: enclave counters, N ≥ 2f + 1.

Every formal object of the dissertation is one immutable type, and every
layer is a pure function of the layer below it. The trust model is a type
parameter (the evidence carrier) that Layer 4 never inspects.

## Layout

```
src/main/scala
  domain/       Step, Slot, CollapsedSlot, Block, Body, WireMessage,
                QuorumEvidence, QuorumCertificate
  crypto/       Evidence, SoftwareEvidence, TeeEvidence
  blockchain/   ValidatorsSet, TrustModel (StdTrust, TeeTrust)
  layers/       L1 Accumulation, L2 Projection, L3 QuorumQueries,
                L4 ProtocolLogic and Tendermint, L5 StabilizationEngine
  bench/        the four tests (ReplicasBenchmark, DelayBenchmark,
                BatchBenchmark, HeightsBenchmark), BenchmarkApp,
                Network, Runner, Fork, Setup, Signer, Table,
                experiments/ with one class per test
src/test/scala
  harness/      AdversarialHarness and the simulator of the system model
  props/        L1Spec to L5Spec, the properties shown in the dissertation
  contracts/    the contract suites K1 to K5, every item of every contract
```

## Validation

Validation runs on two tiers.

Properties (`src/test/scala/props`, ScalaCheck) are a short selection of
the main results of each layer, one predicate per result, drawn on fresh
random seeds at every run. They are the ones shown in Chapter 5:

- L1: convergence (confluence) and the semilattice laws, under both models.
- L2: equivocation hiding under Θ_std, non-equivocation under Θ_tee.
- L3: quorum validity and query convergence, under both models.
- L4 under Θ_std: lock invariant, lock safety, agreement and integrity.
- L4 under Θ_tee: correct replicas keep the lock invariant, but a Byzantine
  validator shared by two quorums breaks its lock and two correct replicas
  decide different blocks at one height.
- L5: termination, under both models.

Contract suites (`src/test/scala/contracts`, ScalaTest) check every law
of Exp_i and every item of Thm_i of each contract, under both trust models
with f = 1 and f = 2, over a fixed range of seeds so that every failure is
reproducible. Faults are never injected by hand: the harness simulates f
Byzantine validators that equivocate, propose invalid blocks, forge
justifications and relay certificates, over a network that reorders and
duplicates, and each test states what must hold in the resulting executions.
Where an item needs a rare shape of execution, the test scripts it message by
message.

## Running

Requires JDK 21 and sbt.

```
sbt test                                   # properties and contract suites
sbt "testOnly props.*"                     # only the properties
sbt "testOnly contracts.*"                 # only the contract suites
```

The evaluation of Chapter 5 is four tests, each a program of its own and
each made for both trust models. Under sbt every `runMain` forks a new
virtual machine, so no test inherits the JIT state left by another:

```
sbt "runMain bench.ReplicasBenchmark"   # test 1, replica scaling
sbt "runMain bench.DelayBenchmark"      # test 2, std against tee under delay
sbt "runMain bench.BatchBenchmark"      # test 3, std against tee under batching
sbt "runMain bench.HeightsBenchmark"    # test 4, height scaling from a cold JVM
```

## Benchmark

The benchmark runs N correct replicas on a fault free network, in rounds. In
a round every replica applies F to what was delivered to it, and everything
all replicas emit in that round is delivered to every replica together, after
one network delay. The delay is paid once per round whatever the batch size:
a message emitted early in a round arrives at the same time as one emitted
late, exactly as if the whole round were one batch. The batch size only
changes how many times F runs over the same delivered set:

- batch 1: one F call per message;
- batch k: one F call per chunk of k messages;
- batch round: one F call for the whole round.

Θ_std and Θ_tee are always compared at equal resilience f, so Θ_std runs
N = 3f + 1 replicas and Θ_tee runs N = 2f + 1. Each test gives the replicas
of Θ_std; `faultsFor(n) = (n − 1) / 3` turns them into f, so 10, 20, 40 and
80 replicas become f = 3, 6, 13 and 26 (Θ_std with 10, 19, 40 and 79
replicas, Θ_tee with 7, 13, 27 and 53).

### Code

- `ReplicasBenchmark`, `DelayBenchmark`, `BatchBenchmark`,
  `HeightsBenchmark`: the four tests, each with its parameters and its
  `Settings`.
- `BenchmarkApp`: what the four tests share, and `Settings`.
- `experiments/`: `ReplicaScaling`, `BatchingUnderDelay`, `Batching` and
  `HeightScaling`, the table each test builds.
- `Network`: the replicas and the round model above.
- `Runner`: medians, warm-up and the execution mode of a test.
- `Fork`: one run in a child virtual machine that starts cold (`ForkedRun`
  is its entry point).
- `Setup`: `Trust`, `Batch`, `Setup` and the `Stats` of a run.
- `Table`: prints each result as an ASCII table, and as LaTeX rows on demand.
- `Signer`: ideal signatures, with the enclave counter under Θ_tee.

### Parameters

Each test sets its parameters in its own object; change them and rerun, no
other code needs to change.

`Settings`, per test:

- `runs` (3): runs per row; the row reports the median by latency.
- `warmupHeights` (100): heights per warm-up run, 0 for none.
- `parallel` (true): one fiber per replica.
- `sleepDelay` (true): pay the delay with a real sleep; false adds it to a
  virtual clock instead, which gives the same latency without waiting.
- `freshJvm` (false): every run in a child virtual machine of its own,
  started cold, with no warm-up.
- `latex` (false): also print the table as LaTeX rows.

The tests:

1. `ReplicasBenchmark`: 10, 20, 40 and 80 replicas, 1000 heights, full
   batching, no delay.
2. `DelayBenchmark`: 20 replicas, delays of 0, 2, 5, 10 and 20 ms, batch 10,
   100 heights.
3. `BatchBenchmark`: 40 replicas, batches of 1, 5, 10, 20 and 40, no delay,
   1000 heights.
4. `HeightsBenchmark`: 20 replicas, 100, 200, 400, 800, 1600, 3200 and 6400
   heights, full batching, no delay. It sets `freshJvm = true` and
   `warmupHeights = 0`: every run starts in a new virtual machine, so each
   row shows the cost per height of a JVM that has decided only that many
   heights, and the table shows the warm-up of the JIT.

### Columns

- `msgs/h`: messages broadcast per height.
- `F calls/h`: calls of F per height, summed over replicas.
- `cpu ms/h`: time inside F per height, summed over replicas.
- `crit ms/h`: per round, the time of the slowest replica, summed: the
  compute on the critical path when each replica has its own machine.
- `net ms/h`: rounds per height times the delay.
- `latency ms/h`: time to decide a height, network included.
- `heights/s`: throughput, heights decided per second.

### Results

Measured on a laptop with an AMD Ryzen 7 7840HS (8 cores, 16 threads) and
32 GB of RAM. These are the values reported in Chapter 5. Every table
compares Θ_std and Θ_tee at the same f; the N of each model is in its rows.

Test 1, replica scaling. Every replica calls F three times per height under
full batching. Latency grows faster than N, since each call works over N
validators, and the ratio between the models approaches the square of their
ratio of replicas, about 2.2x at f = 13.

```
Test 1. Replica scaling (std against tee at equal f)
heights 1000, batch round, delay 0 ms, median of 3, one fiber per replica, real delay
+----+-------+----+--------+--------+-----------+----------+-----------+--------------+-----------+--------+
| f  | trust | N  | quorum | msgs/h | F calls/h | cpu ms/h | crit ms/h | latency ms/h | heights/s | vs tee |
+====+=======+====+========+========+===========+==========+===========+==============+===========+========+
|  3 | std   | 10 |      7 |   21.0 |      30.0 |    0.428 |     0.061 |        0.163 |    6141.9 |  1.36x |
|  3 | tee   |  7 |      4 |   15.0 |      21.0 |    0.223 |     0.046 |        0.120 |    8362.5 |  1.00x |
+----+-------+----+--------+--------+-----------+----------+-----------+--------------+-----------+--------+
|  6 | std   | 19 |     13 |   39.0 |      57.0 |    1.312 |     0.126 |        0.303 |    3305.7 |  1.59x |
|  6 | tee   | 13 |      7 |   27.0 |      39.0 |    0.565 |     0.079 |        0.190 |    5269.8 |  1.00x |
+----+-------+----+--------+--------+-----------+----------+-----------+--------------+-----------+--------+
| 13 | std   | 40 |     27 |   81.0 |     120.0 |    7.271 |     0.398 |        0.748 |    1337.1 |  2.25x |
| 13 | tee   | 27 |     14 |   55.0 |      81.0 |    2.027 |     0.121 |        0.332 |    3013.9 |  1.00x |
+----+-------+----+--------+--------+-----------+----------+-----------+--------------+-----------+--------+
| 26 | std   | 79 |     53 |  159.0 |     237.0 |   29.356 |     1.077 |        2.158 |     463.4 |  1.98x |
| 26 | tee   | 53 |     27 |  107.0 |     159.0 |   12.986 |     0.567 |        1.091 |     916.8 |  1.00x |
+----+-------+----+--------+--------+-----------+----------+-----------+--------------+-----------+--------+
```

Test 2, std against tee under delay. A height costs three delays under both
models, so once the delay dominates the ratio goes to about 1.01x; batch 10
gives both models five F calls per replica per height.

```
Test 2. Std against tee under network delay
f 6, heights 100, median of 3, one fiber per replica, real delay
+----------+-------+----+-------+----------+-----------+----------+-----------+--------------+----------+
| delay ms | trust | N  | batch | rounds/h | F calls/h | net ms/h | crit ms/h | latency ms/h | speed-up |
+==========+=======+====+=======+==========+===========+==========+===========+==============+==========+
|        0 | std   | 19 |    10 |      3.0 |      95.0 |    0.000 |     0.186 |        0.438 |    1.00x |
|        0 | tee   | 13 |    10 |      3.0 |      65.0 |    0.000 |     0.114 |        0.296 |    1.00x |
|        2 | std   | 19 |    10 |      3.0 |      95.0 |    6.000 |     0.222 |        8.546 |    1.00x |
|        2 | tee   | 13 |    10 |      3.0 |      65.0 |    6.000 |     0.125 |        8.292 |    1.00x |
|        5 | std   | 19 |    10 |      3.0 |      95.0 |   15.000 |     0.180 |       17.249 |    1.00x |
|        5 | tee   | 13 |    10 |      3.0 |      65.0 |   15.000 |     0.135 |       17.382 |    1.00x |
|       10 | std   | 19 |    10 |      3.0 |      95.0 |   30.000 |     0.221 |       32.614 |    1.00x |
|       10 | tee   | 13 |    10 |      3.0 |      65.0 |   30.000 |     0.130 |       32.231 |    1.00x |
|       20 | std   | 19 |    10 |      3.0 |      95.0 |   60.000 |     0.187 |       62.881 |    1.00x |
|       20 | tee   | 13 |    10 |      3.0 |      65.0 |   60.000 |     0.127 |       62.533 |    1.00x |
+----------+-------+----+-------+----------+-----------+----------+-----------+--------------+----------+
```

Test 3, std against tee under batching. Calls of F fall to their bound of 3N
once the batch covers a whole round; the ratio between the models falls from
3.36x at batch 1 to 1.65x at batch 40.

```
Test 3. Std against tee under batching
f 13, heights 1000, no delay, median of 3, one fiber per replica, real delay
+-------+----+-------+-----------+----------+-----------+--------------+-----------+----------+
| trust | N  | batch | F calls/h | cpu ms/h | crit ms/h | latency ms/h | heights/s | speed-up |
+=======+====+=======+===========+==========+===========+==============+===========+==========+
| std   | 40 |     1 |    3240.0 |   65.045 |     2.470 |        4.964 |     201.4 |    1.00x |
| std   | 40 |     5 |     680.0 |   16.449 |     0.612 |        1.424 |     702.4 |    4.04x |
| std   | 40 |    10 |     360.0 |    9.919 |     0.348 |        0.901 |    1110.4 |    7.09x |
| std   | 40 |    20 |     200.0 |    8.103 |     0.314 |        0.750 |    1333.4 |    7.88x |
| std   | 40 |    40 |     120.0 |    5.734 |     0.233 |        0.593 |    1687.8 |   10.61x |
+-------+----+-------+-----------+----------+-----------+--------------+-----------+----------+
| tee   | 27 |     1 |    1485.0 |   16.469 |     0.767 |        1.476 |     677.5 |    1.00x |
| tee   | 27 |     5 |     351.0 |    5.079 |     0.259 |        0.576 |    1735.9 |    2.96x |
| tee   | 27 |    10 |     189.0 |    3.440 |     0.159 |        0.424 |    2359.2 |    4.81x |
| tee   | 27 |    20 |     135.0 |    2.639 |     0.127 |        0.357 |    2800.2 |    6.05x |
| tee   | 27 |    40 |      81.0 |    2.693 |     0.147 |        0.360 |    2780.1 |    5.23x |
+-------+----+-------+-----------+----------+-----------+--------------+-----------+----------+
```

Test 4, height scaling from a cold JVM. Every run starts in a fresh virtual
machine with no warm-up, so the cost per height falls as the JIT compiles F:
from 2.15 to 0.39 ms under std and from 1.90 to 0.31 ms under tee between
100 and 6400 heights. The workload per height is constant, since the storage
of Layer 1 is indexed by height, so the whole gain comes from the JIT.

```
Test 4. Height scaling (std against tee)
f 6, batch round, delay 0 ms, median of 3, one fiber per replica, real delay, fresh JVM per run, no warm-up
+---------+-------+----+---------+----------+-----------+--------------+-----------+
| heights | trust | N  | total s | cpu ms/h | crit ms/h | latency ms/h | heights/s |
+=========+=======+====+=========+==========+===========+==============+===========+
|     100 | std   | 19 |   0.215 |   11.911 |     1.012 |        2.153 |     464.4 |
|     100 | tee   | 13 |   0.190 |    9.174 |     0.935 |        1.904 |     525.2 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|     200 | std   | 19 |   0.305 |    8.003 |     0.681 |        1.525 |     655.5 |
|     200 | tee   | 13 |   0.256 |    5.606 |     0.572 |        1.282 |     780.3 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|     400 | std   | 19 |   0.474 |    5.147 |     0.481 |        1.185 |     843.7 |
|     400 | tee   | 13 |   0.372 |    3.736 |     0.391 |        0.931 |    1074.2 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|     800 | std   | 19 |   0.644 |    3.565 |     0.319 |        0.806 |    1241.3 |
|     800 | tee   | 13 |   0.531 |    2.431 |     0.260 |        0.664 |    1506.3 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|    1600 | std   | 19 |   0.951 |    2.750 |     0.249 |        0.594 |    1682.9 |
|    1600 | tee   | 13 |   0.828 |    1.640 |     0.196 |        0.518 |    1931.3 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|    3200 | std   | 19 |   1.536 |    2.279 |     0.202 |        0.480 |    2083.4 |
|    3200 | tee   | 13 |   1.257 |    1.240 |     0.147 |        0.393 |    2546.4 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
|    6400 | std   | 19 |   2.500 |    1.878 |     0.176 |        0.391 |    2560.2 |
|    6400 | tee   | 13 |   1.953 |    1.019 |     0.133 |        0.305 |    3277.5 |
+---------+-------+----+---------+----------+-----------+--------------+-----------+
```

## Formatting

The code is formatted with scalafmt (`.scalafmt.conf`: 80 columns, no
alignment).

## License

Copyright (c) 2026 Francisco Silveira. All rights reserved.

This repository is public so that the dissertation can be read together with
its code. Reading it does not grant any right to use it: copying, modifying,
distributing or using this code, in whole or in part, requires the prior
written permission of the author. See [LICENSE](LICENSE).
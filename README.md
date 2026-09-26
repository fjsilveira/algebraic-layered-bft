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
  bench/        IdealNetworkBenchmark
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
sbt "runMain bench.IdealNetworkBenchmark"  # the evaluation of Chapter 5
```

## Benchmark configuration

`IdealNetworkBenchmark` runs N correct replicas on a fault-free network. What
it sweeps is set by constants at the top of the `IdealNetworkBenchmark`
object in `src/main/scala/bench/IdealNetworkBenchmark.scala`; change them and
rerun, no other code needs to change.

- `Heights`, `WarmupHeights` (1000, 100): heights decided per run and per
  warm-up run.
- `Runs` (3): runs per row; the row reports the median.
- `Replicas` (10, 20, 40): the values of N for the scaling experiment.
- `Faults` (13): the values of f for Θ_std (N = 3f + 1) against Θ_tee
  (N = 2f + 1).
- `DelaysMs` (0, 2, 5, 10, 20): the network delays per step, in milliseconds.
- `Batches` (1, 5, 10, 20, 40) and `BatchReplicas` (40): the batch sizes and
  the N of the batching experiment.
- `RunScaling`, `RunTrustAndDelay`, `RunBatching` (all true): which
  experiments run.

Each experiment prints its rows as `&`-separated values, ready for a LaTeX
table.

## Formatting

The code is formatted with scalafmt (`.scalafmt.conf`: 80 columns, no
alignment).

## License

Copyright (c) 2026 Francisco Silveira. All rights reserved.

This repository is public so that the dissertation can be read together with
its code. Reading it does not grant any right to use it: copying, modifying,
distributing or using this code, in whole or in part, requires the prior
written permission of the author. See [LICENSE](LICENSE).

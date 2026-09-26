package contracts

import blockchain.{StdTrust, TeeTrust, TrustModel}
import crypto.{Evidence, TeeEvidence}
import domain.*
import layers.*
import harness.*
import org.scalatest.funsuite.AnyFunSuite
import scodec.bits.ByteVector
import java.security.PublicKey
import scala.util.Random

/** Contract K_3 of the quorum queries: the laws of Exp_3 and the items of
  * Corollary (Quorum Queries), which make up Thm_3, each with the results it
  * collects.
  *
  * Every configuration runs at the least N its trust model allows, where the
  * quorum threshold is tightest: Θ_std at N = 3f + 1 (Q_size = 2f + 1) and
  * Θ_tee at N = 2f + 1 (Q_size = f + 1), for f = 1 and f = 2, with exactly f
  * Byzantine validators.
  */
class QuorumQueriesSpec extends AnyFunSuite:

  private val stdConfigs = Vector(1, 2).map(Configuration.minimalStd)
  private val teeConfigs = Vector(1, 2).map(Configuration.minimalTee)

  private val Runs = 40

  private type State[E <: Evidence] = Accumulation[String, E]
  private type Msg[E <: Evidence] = WireMessage[String, E]
  private type Coordinate = (Long, Long, Step)

  // shared by the checks

  /** Step coordinates asked about: the domain of the generator, plus one height
    * and one round beyond it.
    */
  private def coordinates(c: Configuration[?]): Vector[Coordinate] =
    for
      h <- (0L to c.gen.domain.heights).toVector
      r <- 0L to c.gen.domain.rounds
      s <- Step.values.toVector
    yield (h, r, s)

  private def heightsAndRounds(c: Configuration[?]): Vector[(Long, Long)] =
    coordinates(c).map((h, r, _) => (h, r)).distinct

  /** The final state of one replica of a history. */
  private def replica[E <: Evidence](
      c: Configuration[E],
      rnd: Random,
      history: Vector[Msg[E]]
  ): State[E] =
    Accumulation.of(c.gen.replica(rnd, history, rnd.nextInt(10)))

  /** Every state of three replicas, each with its own admitted set, schedule
    * and duplicates.
    */
  private def statesOfReplicas[E <: Evidence](
      c: Configuration[E],
      rnd: Random,
      history: Vector[Msg[E]]
  ): Vector[State[E]] =
    Vector.fill(3)(c.gen.replica(rnd, history, rnd.nextInt(10)))
      .flatMap(Replica.statesAlong)

  /** Whether the state holds some message at height h and round r. */
  private def holdsAt(l: State[?], h: Long, r: Long): Boolean =
    l.at(h).keys.exists(_.round == r)

  /** |W(L_i, h, r)|, computed from reads, independently of syncRound. */
  private def witnessCount[E <: Evidence](
      c: Configuration[E],
      l: State[E],
      h: Long,
      r: Long
  ): Int =
    c.validators.members.count { v =>
      Step.values.exists(s => c.queries.readSlot(l, Slot(h, r, s, v)).isDefined)
    }

  /** Every certificate the queries return on any state of three replicas. */
  private def returnedCertificates[E <: Evidence](
      c: Configuration[E],
      rnd: Random,
      history: Vector[Msg[E]]
  ): Vector[QuorumCertificate[String, E]] =
    for
      l <- statesOfReplicas(c, rnd, history)
      (h, r, s) <- coordinates(c)
      qc <- c.queries.evalQC(l, h, r, s)
    yield qc

  // System model: trust configurations

  /** Θ = (π, N, Q_size) with Q_size = N − f. At the least N the thresholds are
    * 2f + 1 and f + 1, and maxFaulty(N) is the largest f the resilience bound
    * allows for every N.
    */
  private def thresholds(trust: TrustModel[?], least: Int => Int): Unit =
    for f <- 0 to 10 do
      val n = trust.minValidators(f)
      assert(trust.maxFaulty(n) == f, s"N=$n")
      assert(trust.quorumSize(n) == least(f), s"N=$n")
    for n <- 1 to 40 do
      val f = trust.maxFaulty(n)
      val tight = trust.minValidators(f) <= n && trust.minValidators(f + 1) > n
      assert(tight, s"N=$n")
      assert(trust.quorumSize(n) == n - f, s"N=$n: correct ones are no quorum")

  // Exp_3 laws

  /** readSlot at ω returns the pair of an admitted message at ω, or ⊥. */
  private def readSlotLaw[E <: Evidence](c: Configuration[E]): Unit =
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val l = replica(c, rnd, history)
      for w <- c.gen.slotsOf(history) :+ c.gen.unpopulatedSlot do
        assert(c.queries.readSlot(l, w).forall(l(w).contains), s"$w")
    }

  /** syncRound returns ⊥ or a higher round, one where the state holds a
    * message, which has at least f + 1 witnesses and above which no round has
    * as many.
    */
  private def syncRoundLaw[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    val top = c.gen.domain.rounds.toLong
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      for
        l <- statesOfReplicas(c, rnd, history)
        (h, r) <- heightsAndRounds(c)
      do
        def synced(r2: Long) = witnessCount(c, l, h, r2) >= c.f + 1
        c.queries.syncRound(l, h, r) match
          case None =>
            for r2 <- (r + 1) to top do
              assert(!synced(r2), s"($h, $r): round $r2 has f + 1 witnesses")
          case Some(r2) =>
            checked += 1
            assert(r2 > r && holdsAt(l, h, r2) && synced(r2), s"($h, $r)")
            for r3 <- (r2 + 1) to top do
              assert(!synced(r3), s"($h, $r): $r2 is not the highest")
    }
    assert(checked > 0, "syncRound never returned a round")

  /** evalQC returns ⊥ or a certificate at the coordinate asked. */
  private def evalQCLaw[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    Seeds.foreach(Runs) { rnd =>
      val l = replica(c, rnd, c.gen.votingHistory(rnd))
      for (h, r, s) <- coordinates(c); qc <- c.queries.evalQC(l, h, r, s) do
        checked += 1
        assert(qc.slot == CollapsedSlot(h, r, s), s"($h, $r, $s)")
    }
    assert(checked > 0, "evalQC never returned a certificate")

  /** highestQC returns evalQC at the round asked if it is not ⊥, and otherwise
    * ⊥ or evalQC at the same height and step and the highest lower round where
    * it is not ⊥.
    */
  private def highestQCLaw[E <: Evidence](c: Configuration[E]): Unit =
    var fellBack = 0
    Seeds.foreach(Runs) { rnd =>
      val l = replica(c, rnd, c.gen.votingHistory(rnd))
      val q = c.queries
      for (h, r, s) <- coordinates(c) do
        def empty(rs: Seq[Long]) = rs.forall(q.evalQC(l, h, _, s).isEmpty)
        (q.evalQC(l, h, r, s), q.highestQC(l, h, r, s)) match
          case (Some(qc), highest) =>
            assert(highest.contains(qc), s"($h, $r, $s)")
          case (None, None) =>
            assert(empty(0L to r), s"($h, $r, $s): missed a round")
          case (None, Some(qc)) =>
            fellBack += 1
            val rq = qc.slot.round
            assert(qc.slot == CollapsedSlot(h, rq, s) && rq < r, s"($h, $r)")
            assert(q.evalQC(l, h, rq, s).contains(qc), s"($h, $r, $s)")
            assert(empty(rq + 1 to r), s"($h, $r, $s): a higher round")
    }
    assert(fellBack > 0, "highestQC never fell back to a lower round")

  /** hasNilQC holds exactly when evalQC certifies ⊥. */
  private def hasNilQCLaw[E <: Evidence](c: Configuration[E]): Unit =
    var nil = 0
    Seeds.foreach(Runs) { rnd =>
      val l = replica(c, rnd, c.gen.votingHistory(rnd))
      for (h, r, s) <- coordinates(c) do
        val certifiesNil = c.queries.evalQC(l, h, r, s).exists(_.value.isEmpty)
        if certifiesNil then nil += 1
        assert(c.queries.hasNilQC(l, h, r, s) == certifiesNil, s"($h, $r, $s)")
    }
    assert(nil > 0, "no nil certificate was generated")

  /** No query reports on a height and round without messages. */
  private def silentWithoutMessages[E <: Evidence](c: Configuration[E]): Unit =
    Seeds.foreach(Runs) { rnd =>
      val l = replica(c, rnd, c.gen.votingHistory(rnd))
      val q = c.queries
      for (h, r) <- heightsAndRounds(c) if !holdsAt(l, h, r); s <- Step.values
      do
        assert(q.evalQC(l, h, r, s).isEmpty, s"($h, $r, $s)")
        assert(!q.hasNilQC(l, h, r, s), s"($h, $r, $s)")
      for (h, r, s) <- coordinates(c); qc <- q.highestQC(l, h, r, s) do
        assert(holdsAt(l, h, qc.slot.round), s"($h, $r, $s)")
      for (h, r) <- heightsAndRounds(c); r2 <- q.syncRound(l, h, r) do
        assert(holdsAt(l, h, r2), s"($h, $r)")
    }

  // Thm_3, Corollary (Quorum Queries)

  /** Item 1, Lemma (Certificate Soundness): every certificate returned by
    * evalQC or highestQC passes verifyQC, and each entry (v, a, σ) is the
    * message ((h, r, s, v), (x, a), σ) signed by v, which is the read of the
    * state queried at (h, r, s, v).
    */
  private def certificateSoundness[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val signed = history.toSet
      val q = c.queries
      for
        l <- statesOfReplicas(c, rnd, history)
        (h, r, s) <- coordinates(c)
        qc <- q.evalQC(l, h, r, s).toVector ++ q.highestQC(l, h, r, s)
      do
        checked += 1
        assert(q.verifyQC(qc), s"($h, $r, $s)")
        for (v, a, e) <- qc.proof.entries do
          val m = WireMessage(qc.slot.at(v), Body(qc.value, a), e)
          assert(signed(m), s"($h, $r, $s): an entry was never signed")
          val read = q.readSlot(l, m.slot).contains((m.body, m.evidence))
          assert(read, s"($h, $r, $s): an entry is not the read of the state")
    }
    assert(checked > 0, "no certificate was returned")

  /** verifyQC: k ≥ Q_size, signers in V, signers distinct. Checked at the
    * threshold itself.
    */
  private def verifyQCThreshold[E <: Evidence](c: Configuration[E]): Unit =
    val e = c.gen.votingHistory(new Random(0)).head.evidence
    val members = c.validators.members
    val q = c.qSize
    def verifies(signers: Vector[PublicKey]): Boolean =
      val p = QuorumEvidence.from(signers.map(v => (v, "a", e))).get
      c.queries.verifyQC(QuorumCertificate(
        CollapsedSlot(0, 0, Step.PREVOTE),
        None,
        p
      ))
    val outsider = TestValidators.generate(1).head
    assert(!verifies(members.take(q - 1)), "Q_size − 1 signers pass")
    assert(verifies(members.take(q)), "Q_size signers fail")
    assert(verifies(members), "N signers fail")
    assert(!verifies(members.take(q - 1) :+ outsider), "a non-member counts")
    val twice = Vector.fill(2)((members.head, "a", e))
    assert(QuorumEvidence.from(twice).isEmpty, "a repeated signer is accepted")

  /** Item 2(a), Theorems (Quorum Validity) (2): two grounded certificates at
    * one (h, r, s) carry the same value. Grounded certificates are those the
    * queries return at three replicas, in every state, and every certificate
    * admission would accept from the signed messages.
    */
  private def sameCoordinate[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val returned = returnedCertificates(c, rnd, history)
      val assemblable: Map[CollapsedSlot, Set[Option[Block]]] =
        c.gen.certifiable(history)
          .groupBy(m => CollapsedSlot(m.slot.height, m.slot.round, m.slot.step))
          .view
          .mapValues { ms =>
            ms.groupMap(_.body.block)(_.slot.validator)
              .collect { case (x, vs) if vs.distinct.size >= c.qSize => x }
              .toSet
          }
          .toMap
      for (w, xs) <- assemblable do
        assert(xs.size <= 1, s"$w: certificates for ${xs.size} values")
      for (w, qcs) <- returned.groupBy(_.slot) do
        checked += 1
        assert(qcs.map(_.value).distinct.size == 1, s"$w: two values returned")
        val admissible = assemblable.getOrElse(w, Set.empty)
        assert(admissible.contains(qcs.head.value), s"$w")
    }
    assert(checked > 0, "no certificate was returned")

  /** Theorem (Quorum Validity) (1), checked over every pair of Q_size-sets of
    * validators: under Θ_std any two share at least f + 1 validators, so a
    * correct one, and the configuration is admissible; under Θ_tee any two
    * share a validator, but at the least N two may share only Byzantine ones,
    * so it is not.
    */
  private def quorumOverlap(c: Configuration[?], admissible: Boolean): Unit =
    val quorums = c.validators.members.combinations(c.qSize).map(_.toSet)
    val all = quorums.toVector
    val overlaps = for a <- all; b <- all yield a.intersect(b)
    assert(overlaps.forall(_.nonEmpty), "two quorums are disjoint")
    assert(overlaps.map(_.size).min == 2 * c.qSize - c.n)
    assert(overlaps.forall(_.diff(c.byzantine).nonEmpty) == admissible)
    assert(c.queries.admissible == admissible)

  /** Item 2(b), Lemma (Quorum Intersection): under an admissible configuration
    * two grounded certificates at one height, at any rounds and steps, both
    * have an entry of a correct validator.
    */
  private def quorumIntersection[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    Seeds.foreach(Runs) { rnd =>
      val qcs = returnedCertificates(c, rnd, c.gen.votingHistory(rnd)).distinct
      for (_, atH) <- qcs.groupBy(_.slot.height); a <- atH; b <- atH do
        checked += 1
        val shared = a.proof.validators.intersect(b.proof.validators)
        assert(shared.exists(c.correct), s"${a.slot}, ${b.slot}: no correct")
    }
    assert(checked > 0, "no certificate was returned")

  private def tee(w: Slot, x: Option[Block], counter: Long): Msg[TeeEvidence] =
    WireMessage(
      w,
      Body(x, "a"),
      TeeEvidence(ByteVector(counter.toByte), counter)
    )

  /** Remark after Theorem (Quorum Validity, TEE-Assisted): across rounds the
    * enclave says nothing. At N = 2f + 1 a Byzantine validator in a precommit
    * quorum at round 0 and in a prevote quorum at round 1 may be the only
    * validator the two certificates share.
    */
  private def teeAcrossRounds(c: Configuration[TeeEvidence]): Unit =
    val Vector(b, v1, v2) = c.validators.members.take(3): @unchecked
    assert(c.byzantine(b) && c.correct(v1) && c.correct(v2))
    val x = Some(MessageGen.block(1, 0L))
    val y = Some(MessageGen.block(2, 0L))
    val l = Accumulation.of(
      Vector(
        tee(Slot(0, 0, Step.PRECOMMIT, b), x, 0),
        tee(Slot(0, 0, Step.PRECOMMIT, v1), x, 0),
        tee(Slot(0, 1, Step.PREVOTE, b), y, 0),
        tee(Slot(0, 1, Step.PREVOTE, v2), y, 0)
      )
    )
    val precommit = c.queries.evalQC(l, 0, 0, Step.PRECOMMIT)
    val prevote = c.queries.evalQC(l, 0, 1, Step.PREVOTE)
    assert(precommit.map(_.value).contains(x))
    assert(prevote.map(_.value).contains(y))
    val shared = (precommit.toVector ++ prevote)
      .map(_.proof.validators.toSet)
      .reduce(_.intersect(_))
    assert(shared == Set(b), "the certificates share a correct validator")
    assert(!c.queries.admissible)

  /** Theorem (Quorum Validity, TEE-Assisted) (2) on its edge: at N = 3, f = 1,
    * the Byzantine validator signs x and then y at one slot. A replica that
    * admitted y has admitted x before it, so its read is x and no certificate
    * for y is returned there or can be admitted, although without the
    * first-message rule both certificates could be assembled.
    */
  private def teeEnclaveAgreement(c: Configuration[TeeEvidence]): Unit =
    val Vector(b, v1, v2) = c.validators.members.take(3): @unchecked
    val x = Some(MessageGen.block(1, 0L))
    val y = Some(MessageGen.block(2, 0L))
    val bx = tee(Slot(0, 0, Step.PREVOTE, b), x, 0)
    val by = tee(Slot(0, 0, Step.PREVOTE, b), y, 1)
    val v1x = tee(Slot(0, 0, Step.PREVOTE, v1), x, 0)
    val v2y = tee(Slot(0, 0, Step.PREVOTE, v2), y, 0)
    val history = Vector(bx, by, v1x, v2y)

    val a =
      c.queries.evalQC(Accumulation.of(Vector(bx, v1x)), 0, 0, Step.PREVOTE)
    val bb =
      c.queries.evalQC(Accumulation.of(Vector(bx, by, v2y)), 0, 0, Step.PREVOTE)
    assert(a.map(_.value).contains(x))
    assert(bb.isEmpty)

    def signers(ms: Vector[Msg[TeeEvidence]], x: Option[Block]): Int =
      ms.filter(_.body.block == x).map(_.slot.validator).distinct.size
    val both = signers(history, x) >= c.qSize && signers(history, y) >= c.qSize
    assert(both, "without the rule both are assemblable")
    val yAdmitted = signers(c.gen.certifiable(history), y) >= c.qSize
    assert(!yAdmitted, "a certificate for y can be admitted")

  /** Item 3, Lemma (Query Convergence): replicas that admitted the same
    * messages obtain the same result from every query, for every argument.
    */
  private def queryConvergence[E <: Evidence](c: Configuration[E]): Unit =
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val admitted = c.gen.admit(rnd, history)
      val Vector(l1, l2) = Vector.fill(2) {
        Accumulation.of(c.gen.delivery(rnd, admitted, rnd.nextInt(10)))
      }: @unchecked
      val q = c.queries
      for w <- c.gen.slotsOf(history) :+ c.gen.unpopulatedSlot do
        assert(q.readSlot(l1, w) == q.readSlot(l2, w), s"readSlot $w")
      for (h, r) <- heightsAndRounds(c) do
        assert(q.syncRound(l1, h, r) == q.syncRound(l2, h, r), s"sync ($h, $r)")
      for (h, r, s) <- coordinates(c) do
        val at = s"($h, $r, $s)"
        assert(q.evalQC(l1, h, r, s) == q.evalQC(l2, h, r, s), s"evalQC $at")
        assert(q.highestQC(l1, h, r, s) == q.highestQC(l2, h, r, s), at)
        assert(q.hasNilQC(l1, h, r, s) == q.hasNilQC(l2, h, r, s), at)
    }

  /** Item 4(a), Lemma (Query Completeness) (1): once a correct validator's
    * message is admitted, readSlot returns it at its slot in every later state.
    */
  private def completenessRead[E <: Evidence](c: Configuration[E]): Unit =
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val ms = c.gen.delivery(rnd, history, rnd.nextInt(10))
      val states = Replica.statesAlong(ms)
      for (m, i) <- ms.zipWithIndex do
        if c.correct(m.slot.validator) && ms.indexOf(m) == i then
          for l <- states.drop(i + 1) do
            val read = c.queries.readSlot(l, m.slot)
            assert(read.contains((m.body, m.evidence)), s"${m.slot}")
    }

  /** Item 4(b), Lemma (Query Completeness) (2): once the messages at (h, r, s)
    * of every correct validator, all carrying x, are admitted, evalQC returns a
    * certificate for x in every later state.
    */
  private def completenessQuorum[E <: Evidence](c: Configuration[E]): Unit =
    var checked = 0
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      val ms = c.gen.delivery(rnd, history, rnd.nextInt(10))
      val states = Replica.statesAlong(ms)
      val first = ms.zipWithIndex.groupMapReduce(_._1)(_._2)(math.min)
      for (h, r, s) <- coordinates(c) do
        val votes = history.filter { m =>
          m.slot == Slot(
            h,
            r,
            s,
            m.slot.validator
          ) && c.correct(m.slot.validator)
        }
        val xs = votes.map(_.body.block).distinct
        if votes.size == c.correct.size && xs.size == 1 then
          checked += 1
          for l <- states.drop(votes.map(first).max + 1) do
            val qc = c.queries.evalQC(l, h, r, s)
            assert(qc.map(_.value).contains(xs.head), s"($h, $r, $s)")
    }
    assert(checked > 0, "no coordinate where every correct validator agreed")

  /** Item 4(c), Lemma (Query Completeness) (3): while the state holds no
    * message of a correct validator at height h above round r, syncRound(L_i,
    * h, r) = ⊥, however many Byzantine messages lie above r.
    */
  private def completenessSyncRound[E <: Evidence](c: Configuration[E]): Unit =
    var byzantineOnly = 0
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      for
        l <- statesOfReplicas(c, rnd, history)
        (h, r) <- heightsAndRounds(c)
      do
        val above = l.at(h).keys.filter(_.round > r)
        if !above.exists(w => c.correct(w.validator)) then
          if above.nonEmpty then byzantineOnly += 1
          assert(c.queries.syncRound(l, h, r).isEmpty, s"($h, $r)")
    }
    assert(byzantineOnly > 0, "no state with only Byzantine messages above")

  /** Item 4(d), Lemma (Query Completeness) (4): if evalQC certifies x at (h, r,
    * s), the state holds a message of a correct validator at (h, r, s) carrying
    * x.
    */
  private def completenessWitness[E <: Evidence](c: Configuration[E]): Unit =
    Seeds.foreach(Runs) { rnd =>
      val history = c.gen.votingHistory(rnd)
      for
        l <- statesOfReplicas(c, rnd, history)
        (h, r, s) <- coordinates(c)
        qc <- c.queries.evalQC(l, h, r, s)
      do
        val carried = c.correct.exists { v =>
          l(Slot(h, r, s, v)).exists(_._1.block == qc.value)
        }
        assert(carried, s"($h, $r, $s): no correct validator carries it")
    }

  // registration

  test("System model, Θ_std: Q_size = N − f, 2f + 1 at N = 3f + 1")(
    thresholds(StdTrust, f => 2 * f + 1)
  )
  test("System model, Θ_tee: Q_size = N − f, f + 1 at N = 2f + 1")(
    thresholds(TeeTrust, f => f + 1)
  )

  private def contract[E <: Evidence](c: Configuration[E]): Unit =
    val n = c.name
    test(s"K3 Exp (readSlot) [$n]: an admitted pair or ⊥")(readSlotLaw(c))
    test(s"K3 Exp (syncRound) [$n]: the highest synced round")(
      syncRoundLaw(c)
    )
    test(s"K3 Exp (evalQC) [$n]: a certificate at the coordinate")(
      evalQCLaw(c)
    )
    test(s"K3 Exp (highestQC) [$n]: the highest round certified")(
      highestQCLaw(c)
    )
    test(s"K3 Exp (hasNilQC) [$n]: exactly a nil certificate")(
      hasNilQCLaw(c)
    )
    test(s"K3 Exp [$n]: no report without messages")(silentWithoutMessages(c))
    test(s"K3 Thm (1), Lemma (Certificate Soundness) [$n]")(
      certificateSoundness(c)
    )
    test(s"K3 Thm (1), Definition (Certificate Verification) [$n]")(
      verifyQCThreshold(c)
    )
    test(s"K3 Thm (2a), Theorem (Quorum Validity) (2) [$n]")(
      sameCoordinate(c)
    )
    test(s"K3 Thm (3), Lemma (Query Convergence) [$n]")(queryConvergence(c))
    test(s"K3 Thm (4a), Lemma (Query Completeness) (1) [$n]")(
      completenessRead(c)
    )
    test(s"K3 Thm (4b), Lemma (Query Completeness) (2) [$n]")(
      completenessQuorum(c)
    )
    test(s"K3 Thm (4c), Lemma (Query Completeness) (3) [$n]")(
      completenessSyncRound(c)
    )
    test(s"K3 Thm (4d), Lemma (Query Completeness) (4) [$n]")(
      completenessWitness(c)
    )

  stdConfigs.foreach(contract)
  teeConfigs.foreach(contract)

  for c <- stdConfigs do
    test(s"Theorem (Quorum Validity, Software-Only) (1) [${c.name}]")(
      quorumOverlap(c, admissible = true)
    )
    test(s"K3 Thm (2b), Lemma (Quorum Intersection) [${c.name}]")(
      quorumIntersection(c)
    )

  for c <- teeConfigs do
    test(s"Theorem (Quorum Validity, TEE-Assisted) (1) [${c.name}]")(
      quorumOverlap(c, admissible = false)
    )

  test("Theorem (Quorum Validity, TEE-Assisted) (2): the enclave keeps y out")(
    teeEnclaveAgreement(teeConfigs.head)
  )
  test("Remark, Θ_tee: across rounds only a Byzantine validator may be shared")(
    teeAcrossRounds(teeConfigs.head)
  )

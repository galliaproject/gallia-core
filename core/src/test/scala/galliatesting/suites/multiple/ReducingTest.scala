package galliatesting
package suites
package multiple

import gallia._

// ===========================================================================
object ReducingTest extends utest.TestSuite with GalliaTestSuite { import utest._
  private val Reducer0 =
    bobjs(
        bobj("g1" -> "a", "g2" -> 1),
        bobj("g1" -> "b", "g2" -> 2))

  // ---------------------------------------------------------------------------
  private val Reducer1 =
    bobjs(
        bobj("g1" -> 1, "g2" -> 7),
        bobj("g1" -> 3, "g2" -> 4))

  // ---------------------------------------------------------------------------
  private val Reducer2 =
    aobjs(cls("g1".int, "g2".double, "g3".int_, "g4".string))(
        obj("g1" -> 1, "g2" -> 7.7, "g3" -> 10, "g4" -> "foo1"),
        obj("g1" -> 3, "g2" -> 4.4,             "g4" -> "foo3"),
        obj("g1" -> 1, "g2" -> 7.7, "g3" -> 20, "g4" -> "foo1"))

  // ===========================================================================
  val tests = Tests {
    test(Reducer0.reduce("g1", "g2").wit(_.flattened).check(bobj("g1" -> Seq("a", "b"), "g2" -> Seq(1, 2))))

    // ---------------------------------------------------------------------------
    test(Reducer1.reduce("g1")      .wit(_.mean).check(bobj("g1" -> 2.0)))
    test(Reducer1.reduce("g1", "g2").wit(_.mean).check(bobj("g1" -> 2.0, "g2" -> 5.5)))

      test(Reducer1.reduceWithCount ("g1", "g2")              .check(bobj("g1" -> 2  , "g2" -> 2  )))
      test(Reducer1.reduceWithSum   ("g1", "g2")              .check(bobj("g1" -> 4  , "g2" -> 11 )))
      test(Reducer1.reduceWithMean  ("g1", "g2")              .check(bobj("g1" -> 2.0, "g2" -> 5.5)))
      test(Reducer1.reduce          ("g1", "g2").wit(_.median).check(bobj("g1" -> 2.0, "g2" -> 5.5)))
      test(Reducer1.reduce          ("g1", "g2").wit(_.stdev ).check(bobj("g1" -> 1.0, "g2" -> 1.5)))

      test(Reducer1.reduce          ("g1", "g2").wit(_.flattened).check(bobj("g1" -> Seq(1, 3), "g2" -> Seq(7, 4))))

      test(Reducer1.reduce(_.firstKey).wit(_.sum).check(bobj("g1" -> 4)))
      test(Reducer1.reduce(_.allKeys ).wit(_.sum).check(bobj("g1" -> 4  , "g2" -> 11 )))

      test(Reducer1.reduce("g1", "g2").wit(_.min  ).check(bobj("g1" -> 1,   "g2" -> 4)))
      test(Reducer1.reduce("g1", "g2").wit(_.range).check(bobj("g1" -> 2,   "g2" -> 3)))
      test(Reducer1.reduce("g1", "g2").wit(_.IQR  ).check(bobj("g1" -> 2.0, "g2" -> 3.0)))

      test(Reducer1.reduce("g1").wit(_.stats).check(bobj("g1" -> bobj(
          _count_all -> 2, "_distinct" -> 2, _mean -> 2.0, _stdev -> 1.4142135623730951, _min -> 1, _max -> 3, _median -> 2.0)))) // uses sample SD

    // ---------------------------------------------------------------------------
    test(Reducer1.reduce("g1".sum_, "g2".mean  )   .check(bobj("g1" -> 4, "g2" -> 5.5)))
    test(Reducer1.reduce("g1".sum_, "g2".median)   .check(bobj("g1" -> 4, "g2" -> 5.5)))
    test(Reducer1.reduce("g1".sum_, "g2".flattened).check(bobj("g1" -> 4, "g2" -> Seq(7, 4))))

    test(Reducer1
      .reduce(
          "g1".aggregates(_.flattened, _.sum),
          "g2".mean)
      .check(
          bobj(
            "g1" -> bobj(_values -> Seq(1, 3), _sum -> 4),
            "g2" -> 5.5) ))

    // ===========================================================================
    val expected1 = bobj("agg" -> 44 /* (3+1) * (4+7) */)

        test(Reducer1.reduceAll { z => // expanded version
            val g1s: HeadV[Seq[Int]] = z.ints("g1")
            val g2s: HeadV[Seq[Int]] = z.ints("g2")

            val sum1: HeadV[Int] = g1s.sum
            val sum2: HeadV[Int] = g2s.sum

            val agg: HeadV[Int] = sum1 times sum2

            agg.dressUp(key = "agg") }
          .check(expected1))

    // ===========================================================================
    val expected2 = bobj("g1s" -> 4, "g2s" -> 11)

      // ---------------------------------------------------------------------------
      test(Reducer1.reduceAll { z =>
          z.ints("g1").sum.dressUp("g1s") merge
          z.ints("g2").sum.dressUp("g2s") }
        .check(expected2))

      // ---------------------------------------------------------------------------
      test(Reducer1.reduceAll { z =>
          z.ints("g1").mapV(_.map(_.toDouble)).sum.dressUp("g1s") merge
          z.ints("g2").mapV(_.map(_.toDouble)).sum.dressUp("g2s") }
        .check(bobj("g1s" -> 4.0, "g2s" -> 11.0)))

      // ---------------------------------------------------------------------------
      test(Reducer1.reduceAll { z =>
          gallia.headO(
            "g1s" -> z.ints("g1").sum,
            "g2s" -> z.ints("g2").sum) }
        .check(expected2))
} }

// ===========================================================================
package galliatesting
package suites
package multiple

import gallia._

// ===========================================================================
object AggregatingTest extends utest.TestSuite with GalliaTestSuite with TestDataO with TestDataS { import utest._
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test("reduce-to-headV (value)") {
      test(Default56.group("f1").by("g").check(bobjs(
        bobj("g" -> 1, "f1" -> Seq("foo1", "foo1")),
        bobj("g" -> 3, "f1" -> Seq("foo3")))))

      test(Default56.aggregateBy("g").as("f1s").using(_.strings("f1")).check(bobjs(
        bobj("g" -> 1, "f1s" -> Seq("foo1", "foo1")),
        bobj("g" -> 3, "f1s" -> Seq("foo3")))))

      test(Default56.aggregateBy("g").using(_.strings("f1")).check(bobjs(
        bobj("g" -> 1, _agg -> Seq("foo1", "foo1")),
        bobj("g" -> 3, _agg -> Seq("foo3")))))

      test(Default56.aggregateBy("g").using { x =>
          x.strings("f1").mapV(_.reduceLeft(_ + _)) }
        .check(bobjs(
          bobj("g" -> 1, _agg -> "foo1foo1"),
          bobj("g" -> 3, _agg -> "foo3"))))

      test(Default56.aggregateBy("g").using { x =>
          x.strings("f1").mapV(_.reduceLeft(_ + _)) concatenate "bar" }
        .check(bobjs(
          bobj("g" -> 1, _agg -> "foo1foo1bar"),
          bobj("g" -> 3, _agg -> "foo3bar"))))

      test(Default56.aggregateBy("g").using { x =>
          x.strings("f1").mapV(_.reduceLeft(_ + _)) concatenate
          x.strings("f2").mapV(_.reduceLeft(_ + _)) }
        .check(bobjs(
          bobj("g" -> 1, _agg -> "foo1foo1foo2foo2"),
          bobj("g" -> 3, _agg -> "foo3foo4")))) }

    // ---------------------------------------------------------------------------
    test("reduce-to-headO (object)") {
      val expected = bobjs(
        bobj("g" -> 1, "f1s" -> "foo1foo1", "f2s" -> "foo2foo2"),
        bobj("g" -> 3, "f1s" -> "foo3"    , "f2s" -> "foo4"))

      test(Default56.aggregateBy("g").using { x =>
          gallia.headO( // explicitly
            "f1s" -> x.strings("f1").concatenateStrings,
            "f2s" -> x.strings("f2").concatenateStrings) }
        .check(expected))

      test(Default56.aggregateBy("g").using { x =>
          ( "f1s" -> x.strings("f1").concatenateStrings,
            "f2s" -> x.strings("f2").concatenateStrings) }
        .check(expected)) }

    // ===========================================================================
    test(Predef.assert(Default52.forceSize == 3)) // f is z

      // ---------------------------------------------------------------------------
      test(Default02b.toSize(f).check(bobj(f -> 3  , g -> 1))) // f is a seq

      // ---------------------------------------------------------------------------
      test(Default02b.toSum (f).check(bobj(f -> 6  , g -> 1))) // f is ints
      test(Default02c.toSum (f).check(bobj(f -> 6.6, g -> 1))) // f is doubles

if (false)//FIXME
Default02b.toSum (f ~> F).check(bobj(F -> 6  , g -> 1)) // f is ints

      // ---------------------------------------------------------------------------
      test(Default02b.toMean(f)                   .check(bobj(f -> 2.0               , g -> 1)))
      test(Default02c.toMean(f).maxDecimals(f, 2).check(bobj(f -> 2.2, g -> 1)))

      // ---------------------------------------------------------------------------
      test(Default02b.toStdev(f).maxDecimals(f, 2).check(bobj(f -> 0.82, g -> 1)))
      test(Default02c.toStdev(f).maxDecimals(f, 2).check(bobj(f -> 0.9 , g -> 1))) // TODO...

    // ===========================================================================
    test(Default52.countBy              (f)           .check(bobjs(bobj(f -> "foo", _count_all -> 2), bobj(f -> "foo2", _count_all -> 1))))
    test(Default52.countBy              (f).asDefault .check(bobjs(bobj(f -> "foo", _count_all -> 2), bobj(f -> "foo2", _count_all -> 1))))
    test(Default52.countBy              (f).as("COUNT").check(bobjs(bobj(f -> "foo", "COUNT"     -> 2), bobj(f -> "foo2", "COUNT" -> 1))))
    test(Default52.count   (g      ).by(f)           .check(bobjs(bobj(f -> "foo", g         -> 2), bobj(f -> "foo2", g     -> 1))))
    test(Default52.count   (g ~> "G").by(f)           .check(bobjs(bobj(f -> "foo", "G"         -> 2), bobj(f -> "foo2", "G"     -> 1))))

    // ---------------------------------------------------------------------------
    test(Default56.countWith(_.count_all).by(g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 3, _count_all -> 1))))
    test(Default56.countBy                  (g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 3, _count_all -> 1))))

      test(Default57.count    (f).by(g)                             .check(bobjs(bobj(g -> 1, f         -> 3), bobj(g -> 2, f         -> 1))))
      test(Default57.countBy                          (g).as("COUNT").check(bobjs(bobj(g -> 1, "COUNT"     -> 3), bobj(g -> 2, "COUNT"     -> 1))))
      test(Default57.countBy                          (g)            .check(bobjs(bobj(g -> 1, _count_all -> 3), bobj(g -> 2, _count_all -> 1))))
      test(Default57.countWith        (_.count_all).by(g)            .check(bobjs(bobj(g -> 1, _count_all -> 3), bobj(g -> 2, _count_all -> 1))))
      test(Default57.aggregate(f).wit(_.count_all).by(g)            .check(bobjs(bobj(g -> 1, f         -> 3), bobj(g -> 2, f         -> 1))))

//if (false)Default57.countPresentBy("g1", "g2")

      test(Default57.countDistinctBy                       (g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 2, _count_all -> 1))))
      test(Default57.countWith        (_.count_distinct).by(g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 2, _count_all -> 1))))
      test(Default57.aggregate(f).wit(_.count_distinct).by(g).check(bobjs(bobj(g -> 1, f         -> 2), bobj(g -> 2, f         -> 1))))

      test(Default57.countPresentBy                       (g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 2, _count_all -> 1))))
      test(Default57.countWith        (_.count_present).by(g).check(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 2, _count_all -> 1))))
      test(Default57.aggregate(f).wit(_.count_present).by(g).check(bobjs(bobj(g -> 1, f         -> 2), bobj(g -> 2, f         -> 1))))

      test(Default57.countWith        (_.count_distinct_present).by(g).check(bobjs(bobj(g -> 1, _count_all -> 1), bobj(g -> 2, _count_all -> 1))))
      test(Default57.aggregate(f).wit(_.count_distinct_present).by(g).check(bobjs(bobj(g -> 1, f         -> 1), bobj(g -> 2, f         -> 1))))

      test(Default57.countWith        (_.count_missing).by(g).check(bobjs(bobj(g -> 1, _count_all -> 1), bobj(g -> 2, _count_all -> 0))))
      test(Default57.aggregate(f).wit(_.count_missing).by(g).check(bobjs(bobj(g -> 1, f         -> 1), bobj(g -> 2, f         -> 0))))

    // ---------------------------------------------------------------------------
    // sum/count/... all by
//FIXME: test(Default57.sumAllBy     (f).test__//(bobjs(bobj(g -> 1, _count_all -> 2), bobj(g -> 2, _count_all -> 1))))

    // ---------------------------------------------------------------------------
    test(Default52  .count(g)       .by(f)                   .check(bobjs(bobj(f -> "foo", g -> 2   ), bobj(f -> "foo2", g -> 1  ))))
    test(Default52  .count(_.lastKey).by(f)                   .check(bobjs(bobj(f -> "foo", g -> 2   ), bobj(f -> "foo2", g -> 1  ))))
    test(Default52  .sum  (g)       .by(f)                   .check(bobjs(bobj(f -> "foo", g -> 2   ), bobj(f -> "foo2", g -> 2  ))))
    test(Default52  .sum  (g ~> "G") .by(f)                   .check(bobjs(bobj(f -> "foo", "G" -> 2   ), bobj(f -> "foo2", "G" -> 2  ))))
    test(Default52ef.mean (g)       .by(f).maxDecimals(g, 2).check(bobjs(bobj(f -> "foo", g -> 1.47), bobj(f -> "foo2", g -> 2.2))))
    test(Default52ef.aggregate(g).wit(_.stdev)      .by(f).maxDecimals(g, 2).check(bobjs(bobj(f -> "foo", g -> 0.52), bobj(f -> "foo2", g -> 0.0))))

    // ===========================================================================
    test("x-each") {
      val input =
          bobjs(
              bobj(f -> "foo1", "g1" -> 1, "g2" -> 6),
              bobj(f -> "foo2", "g1" -> 2, "g2" -> 5),
              bobj(f -> "foo1", "g1" -> 3, "g2" -> 4))

         test(input.sum("g1").by(f).check(
           bobjs(
               bobj(f -> "foo1", "g1" -> 4),
               bobj(f -> "foo2", "g1" -> 2) )))

       // ---------------------------------------------------------------------------
       val expected1 =
           bobjs(
               bobj(f -> "foo1", "_sums" -> bobj("g1" -> 4, "g2" -> 10)),
               bobj(f -> "foo2", "_sums" -> bobj("g1" -> 2, "g2" ->  5 )) )

         test(input.sumEach("g1", "g2").by(f).check(expected1))
         test(input.sumEach(_.tailKeys).by(f).check(expected1))

         test(input.sumEach("g1", "g2").by(f).as("COUNTS").check(
           bobjs(
               bobj(f -> "foo1", "COUNTS" -> bobj("g1" -> 4, "g2" -> 10)),
               bobj(f -> "foo2", "COUNTS" -> bobj("g1" -> 2, "g2" ->  5 )) )))

       val expected2 =
           bobjs(
               bobj(f -> "foo1", "_count_alls" -> bobj("g1" -> 2, "g2" -> 2)),
               bobj(f -> "foo2", "_count_alls" -> bobj("g1" -> 1, "g2" -> 1)) )

        test(input.sumEach  ("g1", "g2").by(f).check(expected1))
        test(input.countEach("g1", "g2").by(f).check(expected2))
        test(input.aggregateEach("g1", "g2").wit(_.sum).by(f).check(expected1)) }

    // ===========================================================================
    test(bobjs(
        bobj(f -> "foo", "g1" -> 1, "g2" -> 1.1),
        bobj(f -> "foo", "g1" -> 2, "g2" -> 2.5),
        bobj(f -> "bar", "g1" -> 3, "g2" -> 5.6))
      .aggregate(
            "g1".count_all,
            "g2".mean)
          .by("f")
        .check(bobjs(
            bobj(f -> "foo", _group -> bobj("g1" -> 2, "g2" -> 1.8)),
            bobj(f -> "bar", _group -> bobj("g1" -> 1, "g2" -> 5.6))) ))
  }

}

// ===========================================================================
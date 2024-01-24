package galliatesting
package suites
package single

// ===========================================================================
object HeadVTest extends utest.TestSuite with GalliaTestSuite { import utest._
  import TestDataO._
  import gallia.{bobj, _vle, headV}

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(headV(3)              .check(             3 ))
    test(headV(3).dressUp(_vle).check(bobj(_vle -> 3)))

    test(headV("foo").concatenate("bar")   .check("foobar"))
    test(headV("foo").concatenate(headV(1)).check("foo1"))

    // ---------------------------------------------------------------------------
    //FIXME: 220920155343 - see https://users.scala-lang.org/t/ambiguous-reference-to-overloaded-definition-with/8817
    test(headV(1.1).combine(headV(1)).using(_ + _).check(2.1))
    test(headV(1.1).plus(1.1)                     .check(2.2))
    //if (false) test(headV(1.1).plus   (headV(1)).check(2.1)) // FIXME: t220916154103
    //if (false) test(headV(1.1).plus(1).check(2.1))

    test(headV(List(1, 2, 3)).display())
    test(headV(1.1).plus(headV(1.1)).display())
    test(headV(1.1).plus      (1.1) .display())

    // ---------------------------------------------------------------------------
    test((headV("foo").dressUp("f")
        merge
          headV(1)    .dressUp("g"))
      .check(Default01))

    // ---------------------------------------------------------------------------
    test("multiple values") {
      test(headV(Seq (1, 2, 3)).format.ensuring(_ == "1\n2\n3\n"))
      test(headV(List(1, 2, 3)).format.ensuring(_ == "1\n2\n3\n"))
      test(headV(     1, 2, 3 ).format.ensuring(_ == "1\n2\n3\n"))
      test(headV(           3 ).format.ensuring(_ ==       "3"))

      test(headV(Seq (1, 2, 3)).format.ensuring(_ == "1\n2\n3\n"))

      // ---------------------------------------------------------------------------
      test(headV(Seq (1, 2, 3))   .count   .check(3))
      test(headV(Seq (1, 2, 3, 2)).distinct.check(Seq(1, 2, 3)))
      test(headV(Seq (Some(1), None, Some(3))).flattened.check(Seq(1, 3)))

      test(headV(Seq (3, 1, 2))   .min   .check(1))
      test(headV(Seq (1, 3, 2))   .max   .check(3))
      test(headV(Seq (1, 3, 2))   .range   .check(2))
      test(headV(Seq (1, 3, 2))   .sum   .check(6))
      test(headV(Seq (1, 3, 2))   .mean  .check(2.0))
      test(headV(Seq (1, 3, 2))   .median.check(2.0))
      test(headV(Seq (1, 3, 2))   .stdev.mapV(x => (x * 100).toInt).check(81))

      // ---------------------------------------------------------------------------
      //      headV(Seq (1, 2, 3)).println()
      //      headV(           3 ).println()
      //
      //      headV(Seq (1, 2, 3)).write("/tmp/here1.gz")
      //      headV(           3 ).write("/tmp/here2.gz")
    }
}}

// ===========================================================================

package galliatesting
package suites
package single

// ===========================================================================
object CopyTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default01.copyEntry(f     ).as(f2    ) check bobj(f -> foo, g -> 1, f2 -> foo))
    test(Default01.copyEntry(f     ).as(f2, f3) check bobj(f -> foo, g -> 1, f2 -> foo, f3 -> foo))
    test(Default01.copyEntry(f ~> F).as(f2, f3) check bobj(F -> foo, g -> 1, f2 -> foo, f3 -> foo))

    // ---------------------------------------------------------------------------
    test(Default03.copyEntry(p |> f ~> F).as(f2, f3) check bobj(p -> bobj(F -> foo, g -> 1, f2 -> foo, f3 -> foo), z -> true)) }
}

// ===========================================================================
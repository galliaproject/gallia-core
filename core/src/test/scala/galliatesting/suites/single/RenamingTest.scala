package galliatesting
package suites
package single

// ===========================================================================
object RenamingTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default01.rename(f).to(F).check(bobj(F -> foo, g -> 1)))
    test(Default01.rename(f ~>  F).check(bobj(F -> foo, g -> 1)))
    test(Default01.rename(f).to(F).check(bobj(F -> foo, g -> 1)))
    test(Default01.rename(f ~>  F).check(bobj(F -> foo, g -> 1)))

    // ---------------------------------------------------------------------------
    test(Default03.rename(p |> f ~>  F).check(bobj(p -> bobj(F -> foo, g -> 1), z -> true)))
    test(Default03.rename(p |> f).to(F).check(bobj(p -> bobj(F -> foo, g -> 1), z -> true)))
    //   Default03.rename(p |> f).test() // shouldn't compile

    // ---------------------------------------------------------------------------
    test(bobj("FooBar"    -> 1) .renameToUpperCase("FooBar").check(bobj("FOOBAR" -> 1)))
    test(bobj(          f -> 1 ).renameToUpperCase(     f)  .check(bobj(          F -> 1 )))
    test(bobj(p -> bobj(f -> 1)).renameToUpperCase(p |> f)  .check(bobj(p -> bobj(F -> 1)))) }}

package galliatesting
package suites
package single

// ===========================================================================
object RetainTest extends utest.TestSuite with GalliaTestSuite { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    val retainIn = TestDataO.Default03b

    test(retainIn.retain(                            "f3", z).check(bobj(                                   "f3" -> 3, z -> true)))
    test(retainIn.retain(p,                                z).check(bobj( p  -> bobj( f1  -> 1,  f2 -> 2),             z -> true)))
    test(retainIn.retain(p |> f1,                          z).check(bobj( p  -> bobj( f1  -> 1),                       z -> true)))
    test(retainIn.retain(         p |> f2,                 z).check(bobj( p  -> bobj(            f2 -> 2),             z -> true)))
    test(retainIn.retain(p |> f1, p |> f2,                 z).check(bobj( p  -> bobj( f1  -> 1,  f2 -> 2),             z -> true)))
    test(retainIn.retain(_.customKeys(_.tail))               .check(bobj(                                   "f3" -> 3, z -> true)))
    test(retainIn.retain(p ~> "P")                           .check(bobj("P" -> bobj( f1  -> 1,  f2 -> 2))))
    test(retainIn.retain(p |> f1 ~> "F1", p |> f2 ~> "F2")   .check(bobj( p  -> bobj("F1" -> 1, "F2" -> 2))))
    test(retainIn.retain(p |> f1 ~> "F1", p |> f2 ~> "F2", z).check(bobj( p  -> bobj("F1" -> 1, "F2" -> 2),            z -> true)))

    test(retainIn.retain(_.index  (0))   .check(bobj(p -> bobj(f1 -> 1, f2 -> 2))))
    test(retainIn.retain(_.indices(0, 1)).check(bobj(p -> bobj(f1 -> 1, f2 -> 2), "f3" -> 3))) } }

// ===========================================================================
package galliatesting
package suites
package single

// ===========================================================================
object SwapTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default01                               .swapEntries          (f, g ) check             bobj(g -> foo, f -> 1))
    test(Default03.transform(_.entity(p)).using(_.swapEntries          (f, g)) check bobj( p  -> bobj(g -> foo, f -> 1), z -> _t))
    test(Default03                               .swapEntries(p ~> "P")(f, g ) check bobj("P" -> bobj(g -> foo, f -> 1), z -> _t))

    // ---------------------------------------------------------------------------
    test(bobj(f1 -> foo1, f2 -> foo2, "g1" -> 1, "g2" -> 2)
      .swapEntries(
          'f1  -> 'g1 ,
          "f2" -> 'g2)
        .check(
            bobj("g1" -> foo1, "g2" -> foo2, f1 -> 1, f2 -> 2)) )
  } }

// ===========================================================================
package galliatesting
package suites
package single

// ===========================================================================
object RemoveTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    //   Default01.remove(f ~> "F") -- must not compile
    test(Default01.remove(           f )   check Default00)
    test(Default01.remove(_.explicit(f))   check Default00)
    test(Default01.remove(_.index  (0))    check Default00)
    test(Default01.remove(_.indices(0, 1)) metaError ErrorId.NoFieldsLeft)
    test(Default01.remove(f, g)            metaError ErrorId.NoFieldsLeft)

    // ---------------------------------------------------------------------------
    test(Default06.remove(_.allBut(f1    )) check bobj(f1 -> foo))
    test(Default06.remove(_.allBut(f1, f2)) check bobj(f1 -> foo, f2 -> foo))

    test(Default06.remove(f1, f2) check Default00)

    // ---------------------------------------------------------------------------
    //   Default03.remove(p |> f ~> "F") -- shouldnt compile anymore
    test(Default03.remove(p |> f) check bobj(p -> Default00, z -> true)) } }

// ===========================================================================
package galliatesting
package suites
package single

// ===========================================================================
object MiscSingleTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests { // all the stray test found, to be merged into their proper parent test if possible

    // ---------------------------------------------------------------------------
    // split:
    test(bobj(f -> "foo,bar,baz", g -> 1).split(f).by(",") check bobj(f -> Seq(foo, bar, baz), g -> 1))

    // ---------------------------------------------------------------------------
    test("misc string ops"){ val in = Default01
      test(in.transform(_.string(f)).using(_.toUpperCase)     check bobj(f -> "FOO", g -> 1))
      test(in                               .toUpperCase  (f) check bobj(f -> "FOO", g -> 1))
      test(in                               .reverseString(f) check bobj(f -> "oof", g -> 1))

      test(TestDataO.Default15m.noop(_.toUpperCase(f))) }

    // ===========================================================================
    //test(Default03.add(p |> h -> true).test(bobj(f -> foo, g -> 1, h -> true, h2 -> 3)))
//FIXME: add nested, add objct + with cc

    // ===========================================================================
    //bobj(p -> bobj(f1 -> 1, f2 -> 2), f3 -> 3, z -> true).retain( f3, z) check bobj(f3 -> 4, z -> true)  // intentional error

    // ===========================================================================
    test { val in = Default09
      //in.noop(_.retain(_.allKeys))

      test(in.remove(_.explicit(f)) check bobj(p -> Default01, z -> true))
      test(in.retain(_.explicit(f)) check bobj(                            f -> foo))
    /*in.remove(_.explicit(f ~> F)) check ? -- no longer valid */ }

    // ===========================================================================
    test { val in = Default01
      test(in.rename(f ~> F)        check bobj(F -> foo, g -> 1))
      test(in.retain(f ~> F)        check bobj(F -> foo))
      test(in.remove(f)             check bobj(g -> 1))
      test(in.remove(_.explicit(f)) check bobj(g -> 1))
      test(in.remove(_.index(0))    check bobj(g -> 1)) } } }

// ===========================================================================

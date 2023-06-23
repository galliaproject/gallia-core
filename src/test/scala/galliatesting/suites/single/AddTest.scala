package galliatesting
package suites
package single

// ===========================================================================
object AddTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default01.add(f ->     "x"                        ) metaError ErrorId.FieldAlreadyExists)
    test(Default01.add(f ->     bobj(z -> 1)               ) metaError ErrorId.FieldAlreadyExists)
    test(Default01.add(p ->     bobj(z -> 1)               ) check bobj(f -> foo, g -> 1, p ->     bobj(z -> 1)))
    test(Default01.add(p -> Seq(bobj(z -> 1), bobj(z -> 2))) check bobj(f -> foo, g -> 1, p -> Seq(bobj(z -> 1), bobj(z -> 2))))

    // ---------------------------------------------------------------------------
    test(Default01.add(f -> "FOO"               ) metaError ErrorId.FieldAlreadyExists)
    test(Default01.add(z -> "z1", z -> "z2"     ) metaError ErrorId.DuplicateKeys)
    test(Default01.add(z -> new java.io.File("")) metaError ErrorId.UnsupportedTlSubtype)

    // ---------------------------------------------------------------------------
    //TODO: t210111113206 - allow?
    // Default01.add(p |> f -> "x")

    test(Default01.add(h -> _t)            check bobj(f -> foo, g -> 1, h -> _t))
    test(Default01.add(h -> _t, "h2" -> 3) check bobj(f -> foo, g -> 1, h -> _t, "h2" -> 3))
    test(Default01.addId("myid")           check bobj(f -> foo, g -> 1, _id -> "myid"))

    // ---------------------------------------------------------------------------
    test(Default01.add(p ->     TestMeta.Foo("aa", "AA"))                                check bobj(f -> foo, g -> 1, p ->     bobj("a" -> "aa" , "A" -> "AA")))
    test(Default01.add(p -> Seq(TestMeta.Foo("aa1", "AA1"), TestMeta.Foo("aa2", "AA2"))) check bobj(f -> foo, g -> 1, p -> Seq(bobj("a" -> "aa1", "A" -> "AA1"), bobj("a" -> "aa2", "A" -> "AA2")))) } }

// ===========================================================================
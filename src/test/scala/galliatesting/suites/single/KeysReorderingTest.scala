package galliatesting
package suites
package single

// ===========================================================================
object KeysReorderingTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._ // 230616163337
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test { Default01.reorderKeys           (_.reverse).check(                     bobj(g -> 1, f -> "foo") ) }
    test { Default01.reverseKeyOrder                  .check(                     bobj(g -> 1, f -> "foo") ) }
    test { Default03.reorderKeysRecursively(_.reverse).check(bobj(z -> true, p -> bobj(g -> 1, f -> "foo"))) }

    test { Default01.reorderKeys           (_.tail).metaError[_Error.InvalidKeyReordering] }

    test { Default06a.reorderAsFirstKeys(_.lastKey)        .check(bobj(g -> 1, f1 -> "foo1" , f2 -> "foo2")) }
    test { Default06a.reorderAsFirstKeys(_.explicit(g))    .check(bobj(g -> 1, f1 -> "foo1" , f2 -> "foo2")) }
    test { Default06a.reorderAsFirstKeys(           g)     .check(bobj(g -> 1, f1 -> "foo1" , f2 -> "foo2")) }
    test { Default06a.reorderAsFirstKeys(           g, f2 ).check(bobj(g -> 1,                f2 -> "foo2" , f1 -> "foo1")) }
    test { Default06a.reorderAsFirstKeys(_.explicit(g, f2)).check(bobj(g -> 1,                f2 -> "foo2" , f1 -> "foo1")) }

    test { Default06a.reorderAsLastKeys(_.firstKey)        .check(bobj( f2 -> "foo2", g -> 1, f1 -> "foo1")) }
    test { Default06a.reorderAsLastKeys(_.explicit(f1))    .check(bobj( f2 -> "foo2", g -> 1, f1 -> "foo1")) }
    test { Default06a.reorderAsLastKeys(           f1)     .check(bobj( f2 -> "foo2", g -> 1, f1 -> "foo1")) }
    test { Default06a.reorderAsLastKeys(           f2, f1 ).check(bobj(               g -> 1, f1 -> "foo1", f2 -> "foo2")) }
    test { Default06a.reorderAsLastKeys(_.explicit(f2, f1)).check(bobj(               g -> 1, f1 -> "foo1", f2 -> "foo2")) } } }

// ===========================================================================
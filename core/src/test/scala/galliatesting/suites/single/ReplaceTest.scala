package galliatesting
package suites
package single

// ===========================================================================
object ReplaceTest extends utest.TestSuite with GalliaTestSuite with TestDataO { import utest._
  import gallia._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test(Default01.replace(f2 -> "FOO"          ) metaError ErrorId.NoSuchField)
    test(Default01.replace(f  -> "z1", f -> "z2") metaError ErrorId.DuplicateKeys)

    test(Default01.replace(f -> "FOO") check bobj(f -> "FOO", g -> 1))
    test(Default01.replace(f ->     3) check bobj(f ->    3 , g -> 1))

    test(Default01.replace(f -> TestMeta.Foo("aa", "AA")) check bobj(f -> bobj("a" -> "aa", "A" -> "AA"), g -> 1)) } }

// ===========================================================================
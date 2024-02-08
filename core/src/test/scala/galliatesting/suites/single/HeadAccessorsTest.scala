package galliatesting
package suites
package single

// ===========================================================================
object HeadAccessorsTest extends utest.TestSuite with GalliaTestSuite with TestDataO with TestDataS { import utest._
  import gallia._
  import gallia.vldt.ErrorId._

  // ===========================================================================
  private val present1 = aobj(
    cls(f.string_, g.int))(
    obj(f-> foo,   g-> 1) )

  // ---------------------------------------------------------------------------
  private val missing1 = aobj(
    cls(f.string_, g.int))(
    obj(           g-> 1) )

  // ---------------------------------------------------------------------------
  private val present2 = aobj(
    cls(f.strings_          , g.int))(
    obj(f-> Seq(foo1, foo2) , g-> 1) )

  // ---------------------------------------------------------------------------
  private val missing2 = aobj(
    cls(f.strings_, g.int))(
    obj(            g-> 1) )

  // ===========================================================================
  val tests = Tests {
    test(Default01     .string(f).check(foo))
    test(Default01.forceString(f).check(foo))

    test(       present1.string(f).metaError(TypeMismatch))
    test(throws(present1.forceString(f)))

    test(present1.     string_(f).check(Some(foo)))
    test(present1.forceString_(f).check(Some(foo) ))

    test(       missing1.string(f).metaError(TypeMismatch))
    test(throws(missing1.forceString(f)))

    test(missing1.     string_(f).check(None))
    test(missing1.forceString_(f).ensuring(_.isEmpty ))

    test(Default02.forceStrings (f).check(     Seq(foo1, foo2)))
  //test(Default02.strings_9(f).ttest(Some(Seq(foo1, foo2))))

    test(present2.forceStrings_(f).check(Some(Seq(foo1, foo2))))
    test(missing2.forceStrings_(f).check(None))

    test(Default01.int(f).metaError(TypeMismatch))

    // ---------------------------------------------------------------------------
    test(Default01 .forceAny (f).check(    foo        : Any))
    test(Default02 .forceAny (f).check(Seq(foo1, foo2): Any))

    test(Default51 .forceAnys(f).check(Seq(foo, foo2): Seq[Any]))
    test(Default52b.forceAnys(f).check(Seq(Seq(foo1, foo2), Seq(foo3, foo4), Seq(foo1, foo2)): Seq[Any]))
  }}

// ===========================================================================
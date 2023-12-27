package galliatesting
package suites

import gallia._
import aptus._

// ===========================================================================
object EnumTest  extends utest.TestSuite with GalliaTestSuite with utils.FormerSandboxImplicits { import utest._
  lazy val aobj1 = AObj(cls(f.string, e.enm("a", "b")),      obj(f -> "foo", e -> "a".e))
  lazy val aobj2 = AObj(cls(f.string, e.enm("a", "b")),      obj(f -> "foo", e -> "b".e))
  lazy val aobj3 = AObj(cls(f.string, e.string),             obj(f -> "foo", e -> "b"  ))
  lazy val aobj4 = AObj(cls(f.string, e.int),                obj(f -> "foo", e ->  1   ))
  lazy val aobj5 = AObj(cls(f.string, e.enm("a", "b", "c")), obj(f -> "foo", e -> "a".e))

  // ---------------------------------------------------------------------------
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test(aobj1.transform(_.enm(e)).using(identity)             ._assert(aobj1))
    test(aobj1.transform(_.enm(e)).using { _.stringValue.size }._assert(aobj4))

    // ---------------------------------------------------------------------------
    test(aobj1.transform(_.enm(e)).using { case EnumValue("a") =>           "b" ; case EnumValue("b") =>           "a"  }._assert(aobj3))
    test(aobj1.transform(_.enm(e)).using { case EnumValue("a") => EnumValue("b"); case EnumValue("b") => EnumValue("a") }._assert(aobj2))
    test(aobj1.transform(_.enm(e)).using { case EnumValue("a") =>            1  ; case EnumValue("b") =>            2   }._assert(aobj4))

    test(aobj1.transform(_.enm(e)).using { case EnumValue("a") => Seq("a".e, "b".e); case EnumValue("b") => Seq("b".e, "a".e) }._assert(
      AObj(cls(f.string, e.enms("a", "b")),      obj(f -> foo, e -> Seq("a".e, "b".e))) ))

    // ---------------------------------------------------------------------------
    test(aobj1.convert(e).toStr.convert(e).toEnum("a", "b", "c")                               ._assert(aobj5))
    test(aobj1.convert(e).toStr.convert(e).toEnum("a", "b", "a")                               ._fail("InvalidEnumStringValues"))

    test(aobj1                                       .modifyEnumValuesFor(e)  .using(_ :+ "c".e) ._assert(aobj5))
    test(aobj1.identity.forEachKey(e).thn((x, k) => x.modifyEnumValuesFor( k) .using(_ :+ "c".e))._assert(aobj5))
    test(aobj1                                       .modifyEnumValuesFor("F").using(_ :+ "a".e)._fail("NoSuchField"))
    test(aobj1                                       .modifyEnumValuesFor(f)  .using(_ :+ "a".e)._fail("NotAnEnumField"))
    test(aobj1                                       .modifyEnumValuesFor(e)  .using(_ :+ "a".e)._fail("InvalidEnumStringValues"))

    // ---------------------------------------------------------------------------
    test(assert(obj(f -> "a".e).enumeratum[TestMeta.MyEnum1](f) == TestMeta.MyEnum1.a))
    test(assert(obj(f -> "a".e).enm                         (f) == EnumValue("a")))

    // ---------------------------------------------------------------------------
    test(Predef.assert(aobj1.forceEnm(e).stringValue == "a"))

    // ---------------------------------------------------------------------------
    // whatever
    test(aobj1.transform(e).using(_.stringValue.toUpperCase)._assert(AObj(cls(f.string, e.string),      obj(f -> "foo", e -> "A"))))

    // ---------------------------------------------------------------------------
    test(typeNode[TestMeta.MyEnum1].flattenedEnumValueNames.ensuring(_ == List("a", "b"))) } }

// ===========================================================================
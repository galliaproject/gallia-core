package galliatesting
package suites
package misc

import gallia.reflect.TypeNode

// ===========================================================================
object ReflectionTest extends utest.TestSuite { import utest._
  import aptus._
  import TestMeta._

  // ===========================================================================
  val ScalaInt = "scala.Int"

  val JavaInt     = "java.lang.Integer"
  val JavaString  = "java.lang.String"
  val JavaFile    = "java.io.File"

  // ---------------------------------------------------------------------------
  type MyStringAlias   = java.lang.String
  type MyJavaIntAlias  = java.lang.Integer
  type MyScalaIntAlias = scala.Int

  // ---------------------------------------------------------------------------
  private val ThisPath: String = this.getClass.getName.stripSuffix("$") // not overly relevant here

  // ===========================================================================
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test("basic") {
      {
        val jv : java.lang.Integer = 1
        val scl: scala.Int         = jv
        val any: Any               = jv

        testClassName(jv .getClass.getName, JavaInt)
        testClassName(scl.getClass.getName, "int")
        testClassName(any.getClass.getName, JavaInt)

        testClassName(generic(jv ), JavaInt)
        testClassName(generic(scl), JavaInt)
        testClassName(generic(any), JavaInt)

        testClassName(scala.Int.box  (scl).getClass.getName, JavaInt)
        testClassName(scala.Int.unbox(jv) .getClass.getName, "int")
      }

      // ---------------------------------------------------------------------------
      {
        val jv : java.lang.String    = "foo"
        val scl: scala.Predef.String = jv
        val any: Any                 = jv

        testClassName(jv .getClass.getName, JavaString)
        testClassName(scl.getClass.getName, JavaString)
        testClassName(any.getClass.getName, JavaString)

        testClassName(generic(jv ), JavaString)
        testClassName(generic(scl), JavaString)
        testClassName(generic(any), JavaString)
      } }

    // ---------------------------------------------------------------------------
    test("basic TypeNode") {
      testTypeNode(gallia.typeNode[scala    .Int]      , TypeNode.ScalaInt)
      testTypeNode(gallia.typeNode[MyScalaIntAlias]    , TypeNode.ScalaInt.alias(s"${ThisPath}.MyScalaIntAlias"))

      testTypeNode(gallia.typeNode[java.lang.Integer]  , TypeNode.JavaInt)
      testTypeNode(gallia.typeNode[MyJavaIntAlias]     , TypeNode.JavaInt.alias(s"${ThisPath}.MyJavaIntAlias"))

      testTypeNode(gallia.typeNode[             String], TypeNode.String)
      testTypeNode(gallia.typeNode[scala.Predef.String], TypeNode.String)
      testTypeNode(gallia.typeNode[java.lang.String]   , TypeNode.String)
      testTypeNode(gallia.typeNode[MyStringAlias]      , TypeNode.String.alias(s"${ThisPath}.MyStringAlias"))

      testTypeNode(gallia.typeNode[java.io.File]       , TypeNode.trivial("java.io.File")) }

    // ---------------------------------------------------------------------------
    test("complex TypeNode") {
      val actual   = gallia.low.typeNode[MyComplexData]
      val Expected = gallia.testing.resourceContent("TypeNodeExample.json").prettyJson

      if (scala.util.Properties.versionNumberString.startsWith("2.12."))
        Predef.assert(
              actual.formatDefault.prettyJson ==     Expected.replace("scala.collection.immutable.Seq", "scala.collection.Seq"),
          Seq(actual.formatDefault.prettyJson, "\n", Expected.replace("scala.collection.immutable.Seq", "scala.collection.Seq")).section2)
      else
        Predef.assert(
              actual.formatDefault.prettyJson ==     Expected,
          Seq(actual.formatDefault.prettyJson, "\n", Expected).section2) }

    // ---------------------------------------------------------------------------
    test("if applicable") { // for union types
      val node = gallia.typeNode[Int]

      assert(node.ifApplicable(_ + 1).apply("foo") == "foo") // ignored
      assert(node.ifApplicable(_ + 1).apply(1)     == 2) } }

  // ===========================================================================
  private def testClassName(actual: String, expected: String): Unit = Predef.assert(actual == expected, actual -> expected)

  // ---------------------------------------------------------------------------
  private def generic[T](t: T): String = t.getClass.getName

  // ---------------------------------------------------------------------------
  private def testTypeNode[T](actual: T, expected: T): Unit =
    Predef.assert(
      actual == expected,
      Seq(actual, expected,
        actual  .toString.splitBy("\n").diff(
        expected.toString.splitBy("\n"))))
}

// ===========================================================================

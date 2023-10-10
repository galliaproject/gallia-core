package galliatesting
package suites
package misc

// ===========================================================================
object ReflectionTest extends utest.TestSuite { import utest._
  import aptus._
  import TestMeta._

  // ===========================================================================
  val tests = Tests {
    test("TypeNode") {
      val actual   = gallia.reflect.low.typeNode[MyComplexData]
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
    test("WeakTypeTagDecorator") { // for union types
      val wttd = new gallia.reflect.WeakTypeTagDecorator[Int]

      assert(wttd.ifApplicable(_ + 1).apply("foo") == "foo") // ignored
      assert(wttd.ifApplicable(_ + 1).apply(1)     == 2) } } }

// ===========================================================================

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
      val actual   = gallia.reflect.TypeLeafParser.parseTypeNode[MyComplexData]
      val Expected = gallia.testing.resourceContent("TypeNodeExample.json").prettyJson

      Predef.assert(
            actual.formatDefault.prettyJson ==     Expected,
        Seq(actual.formatDefault.prettyJson, "\n", Expected).section2) }

    // ---------------------------------------------------------------------------
    test("WeakTypeTagDecorator") {
      import scala.reflect.runtime.universe.weakTypeTag

      assert(gallia.testing.accessPrivate.useWeakTypeTagDecorator(weakTypeTag[Int])   (_.sameType("foo") == false))
      assert(gallia.testing.accessPrivate.useWeakTypeTagDecorator(weakTypeTag[String])(_.sameType("foo") == true)) }
  }

}

// ===========================================================================

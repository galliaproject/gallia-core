package galliatesting
package suites
package misc

import aptus._

// ===========================================================================
object TypeNodeTest extends utest.TestSuite { import utest._
  import gallia.reflect.{TypeNode, TypeLeaf, Field, TypeNodeBuiltIns}

  // ---------------------------------------------------------------------------
  private type MyAliasToCaseClass = TestMeta.Foo

  // ---------------------------------------------------------------------------
  private val FooTypeNode: TypeNode =
    TypeNode(
      leaf = TypeLeaf(s"galliatesting.${gallia.ScalaVersion.companionName("TestMeta")}.Foo", dataClass = true,
        fields = List(Field.string("a"), Field.string("A"))),
      args = List.empty)

  // ---------------------------------------------------------------------------
       class MyAnyVal1(val b: Boolean) extends AnyVal
  case class MyAnyVal2(val b: Boolean) extends AnyVal

  // ===========================================================================
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test("built-ins") {
        test(compare(gallia._typeNode[String]             , TypeNodeBuiltIns.JavaString))
        test(compare(gallia._typeNode[scala.Predef.String], TypeNodeBuiltIns.JavaString))
        test(compare(gallia._typeNode[java.lang.String]   , TypeNodeBuiltIns.JavaString))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[Boolean], TypeNodeBuiltIns.ScalaBoolean))
        test(compare(gallia._typeNode[Int]    , TypeNodeBuiltIns.ScalaInt))
        test(compare(gallia._typeNode[Double] , TypeNodeBuiltIns.ScalaDouble))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[Byte] , TypeNodeBuiltIns.ScalaByte))
        test(compare(gallia._typeNode[Short], TypeNodeBuiltIns.ScalaShort))
        test(compare(gallia._typeNode[Long] , TypeNodeBuiltIns.ScalaLong))
        test(compare(gallia._typeNode[Float], TypeNodeBuiltIns.ScalaFloat))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[BigInt]    , TypeNodeBuiltIns.ScalaMathBigInt))
        test(compare(gallia._typeNode[BigDecimal], TypeNodeBuiltIns.ScalaMathBigDecimal))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[java.time.LocalDate]     , TypeNodeBuiltIns.JavaTimeLocalDate))
        test(compare(gallia._typeNode[java.time.LocalTime]     , TypeNodeBuiltIns.JavaTimeLocalTime))
        test(compare(gallia._typeNode[java.time.LocalDateTime] , TypeNodeBuiltIns.JavaTimeLocalDateTime))
        test(compare(gallia._typeNode[java.time.OffsetDateTime], TypeNodeBuiltIns.JavaTimeOffsetDateTime))
        test(compare(gallia._typeNode[java.time.ZonedDateTime] , TypeNodeBuiltIns.JavaTimeZonedDateTime))
        test(compare(gallia._typeNode[java.time.Instant]       , TypeNodeBuiltIns.JavaTimeInstant))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[java.nio.ByteBuffer]     , TypeNodeBuiltIns.JavaNioByteByffer))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[Seq   [Boolean]]     , TypeNodeBuiltIns.scalaSeq   (TypeNodeBuiltIns.ScalaBoolean)))
        test(compare(gallia._typeNode[Option[Boolean]]     , TypeNodeBuiltIns.scalaOption(TypeNodeBuiltIns.ScalaBoolean)))

        // ---------------------------------------------------------------------------
        test(compare(gallia._typeNode[gallia.EnumValue]  , TypeNodeBuiltIns.GalliaEnumValue))

        test(compare(gallia._typeNode[gallia       .AObj], TypeNodeBuiltIns.GalliaAObj))
        test(compare(gallia._typeNode[gallia       .BObj], TypeNodeBuiltIns.GalliaBObj)) }

    // ---------------------------------------------------------------------------
    test("java") {
        test(compare(gallia._typeNode[java.lang.String]      , TypeNode.trivial("java.lang.String")))
        test(compare(gallia._typeNode[java.lang.Integer]     , TypeNode.trivial("java.lang.Integer")))
        test(compare(gallia._typeNode[java.lang.Boolean]     , TypeNode.trivial("java.lang.Boolean")))
        test(compare(gallia._typeNode[java.io.File]          , TypeNode.trivial("java.io.File"))) }

    // ---------------------------------------------------------------------------
    test("case classes") {
        test(compare(gallia._typeNode[MyAliasToCaseClass], gallia._typeNode[TestMeta.Foo]))
        test(compare(gallia._typeNode[MyAliasToCaseClass], FooTypeNode))

        test(assert(!gallia._typeNode[MyAnyVal1].leaf.dataClass))
        test(assert(!gallia._typeNode[MyAnyVal2].leaf.dataClass)) }

    // ---------------------------------------------------------------------------
    test("complex TypeNode") { import gallia._
      val actual   = gallia._typeNode[TestMeta.MyComplexData]
      val Expected = gallia.testing.resourceContent("TypeNodeExample.json").prettyJson

      val actualFormatted = actual.formatDefault.prettyJson

      if (gallia.ScalaVersion.isScala2)
        Predef.assert(
          actualFormatted.replace("scala.collection.Seq", "scala.collection.immutable.Seq" /* only for 2.12 */) ==
          Expected       .replace("TestMeta$", "TestMeta" /* for both 2.12 and 2.13 */))
      else
        Predef.assert(actualFormatted == Expected) } }

  // ===========================================================================
  def compare(actual: TypeNode, expected: TypeNode) =
    Predef.assert(
      actual == expected,
      Seq(actual, expected).map(_.formatDebug).section2)
}

// ===========================================================================
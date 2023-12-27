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

  // ===========================================================================
  val tests = Tests {

    // ---------------------------------------------------------------------------
    test("built-ins") {
        test(compare(gallia.typeNode[String]             , TypeNodeBuiltIns.JavaString))
        test(compare(gallia.typeNode[scala.Predef.String], TypeNodeBuiltIns.JavaString))
        test(compare(gallia.typeNode[java.lang.String]   , TypeNodeBuiltIns.JavaString))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[Boolean], TypeNodeBuiltIns.ScalaBoolean))
        test(compare(gallia.typeNode[Int]    , TypeNodeBuiltIns.ScalaInt))
        test(compare(gallia.typeNode[Double] , TypeNodeBuiltIns.ScalaDouble))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[Byte] , TypeNodeBuiltIns.ScalaByte))
        test(compare(gallia.typeNode[Short], TypeNodeBuiltIns.ScalaShort))
        test(compare(gallia.typeNode[Long] , TypeNodeBuiltIns.ScalaLong))
        test(compare(gallia.typeNode[Float], TypeNodeBuiltIns.ScalaFloat))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[BigInt]    , TypeNodeBuiltIns.ScalaMathBigInt))
        test(compare(gallia.typeNode[BigDecimal], TypeNodeBuiltIns.ScalaMathBigDecimal))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[java.time.LocalDate]     , TypeNodeBuiltIns.JavaTimeLocalDate))
        test(compare(gallia.typeNode[java.time.LocalTime]     , TypeNodeBuiltIns.JavaTimeLocalTime))
        test(compare(gallia.typeNode[java.time.LocalDateTime] , TypeNodeBuiltIns.JavaTimeLocalDateTime))
        test(compare(gallia.typeNode[java.time.OffsetDateTime], TypeNodeBuiltIns.JavaTimeOffsetDateTime))
        test(compare(gallia.typeNode[java.time.ZonedDateTime] , TypeNodeBuiltIns.JavaTimeZonedDateTime))
        test(compare(gallia.typeNode[java.time.Instant]       , TypeNodeBuiltIns.JavaTimeInstant))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[java.nio.ByteBuffer]     , TypeNodeBuiltIns.JavaNioByteByffer))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[Seq   [Boolean]]     , TypeNodeBuiltIns.scalaSeq   (TypeNodeBuiltIns.ScalaBoolean)))
        test(compare(gallia.typeNode[Option[Boolean]]     , TypeNodeBuiltIns.scalaOption(TypeNodeBuiltIns.ScalaBoolean)))

        // ---------------------------------------------------------------------------
        test(compare(gallia.typeNode[gallia.EnumValue]  , TypeNodeBuiltIns.GalliaEnumValue))

        test(compare(gallia.typeNode[gallia       .AObj], TypeNodeBuiltIns.GalliaAObj))
        test(compare(gallia.typeNode[gallia       .BObj], TypeNodeBuiltIns.GalliaBObj)) }

    // ---------------------------------------------------------------------------
    test("java") {
        test(compare(gallia.typeNode[java.lang.String]      , TypeNode.trivial("java.lang.String")))
        test(compare(gallia.typeNode[java.lang.Integer]     , TypeNode.trivial("java.lang.Integer")))
        test(compare(gallia.typeNode[java.lang.Boolean]     , TypeNode.trivial("java.lang.Boolean")))
        test(compare(gallia.typeNode[java.io.File]          , TypeNode.trivial("java.io.File"))) }

    // ---------------------------------------------------------------------------
    test("case classes") {
        test(compare(gallia.typeNode[MyAliasToCaseClass], gallia.typeNode[TestMeta.Foo]))
        test(compare(gallia.typeNode[MyAliasToCaseClass], FooTypeNode)) }

    // ---------------------------------------------------------------------------
    test("complex TypeNode") { import gallia._ // for .formatDefault on TypeNode
      val actual   = gallia.typeNode[TestMeta.MyComplexData]
      val Expected = gallia.testing.resourceContent("TypeNodeExample.json").prettyJson

      if (gallia.ScalaVersion.isScala2)
        Predef.assert(actual.formatDefault.prettyJson ==     Expected.replace("TestMeta$", "TestMeta"))
      else
        Predef.assert(actual.formatDefault.prettyJson ==     Expected) }
  }

  // ===========================================================================
  def compare(actual: TypeNode, expected: TypeNode) =
    Predef.assert(
      actual == expected,
      Seq(actual, expected).map(_.formatDebug).section2)
}

// ===========================================================================
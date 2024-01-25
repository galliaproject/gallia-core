package galliatesting
package suites
package io
package in

import gallia._

// ===========================================================================
object JsonTest1b
    extends utest.TestSuite with GalliaTestSuite with utils.FormerSandboxImplicits {
  import utest._
  import TimeTestData._

  // ---------------------------------------------------------------------------
  private val  localDateTime2 = "2022-03-16T11:11:45"

  // ===========================================================================
  val tests = Tests {
    test("""{"value": true  }""".read(_.schema("value".boolean))._assert2(bobj("value" -> true )))

    test("""{"value": "foo" }""".read(_.schema("value".string))           ._assert2(bobj("value" -> "foo")))
    test("""{"value": "foo" }""".read(_.schema("value".enm("foo", "oof")))._assert2(aobj("value".enm("foo", "oof"))(obj("value" -> EnumValue("foo")))))

    // ---------------------------------------------------------------------------
    test("numbers-related") {
      test("""{"value": 1 }""".read(_.schema("value".int   ))._assert2(bobj("value" -> 1)))
      test("""{"value": 1 }""".read(_.schema("value".byte  ))._assert2(bobj("value" -> 1.toByte )))
      test("""{"value": 1 }""".read(_.schema("value".short ))._assert2(bobj("value" -> 1.toShort)))
      test("""{"value": 1 }""".read(_.schema("value".long  ))._assert2(bobj("value" -> 1.toLong )))
      test("""{"value": 1 }""".read(_.schema("value".bigInt))._assert2(bobj("value" -> BigInt(1) )))

      test("""{"value": 1.1 }""".read(_.schema("value".double))._assert2(bobj("value" -> 1.1)))
      test("""{"value": 1.1 }""".read(_.schema("value".float ))._assert2(bobj("value" -> 1.1.toFloat)))
      test("""{"value": 1.1 }""".read(_.schema("value".bigDec))._assert2(bobj("value" -> BigDec(1.1)  ))) }

    // ---------------------------------------------------------------------------
    test("time-related") {
      import aptus.String_

      test(s"""{"value": ${localDate    .quote} }""".read(_.schema("value".localDate    ))._assert2(bobj("value" -> localDate    .parseLocalDate)))
      test(s"""{"value": ${localDateTime.quote} }""".read(_.schema("value".localDateTime))._assert2(bobj("value" -> localDateTime.parseLocalDateTime)))
      test(s"""{"value": ${instant      .quote} }""".read(_.schema("value".instant      ))._assert2(bobj("value" -> instant      .parseInstant)))

      test(s"""{"value": ${d .removeIfApplicable(",")} }""".read(_.schema("value".localDate    ))._assert2(bobj("value" -> localDate     .parseLocalDate)))
      test(s"""{"value": ${s .removeIfApplicable(",")} }""".read(_.schema("value".localDateTime))._assert2(bobj("value" -> localDateTime2.parseLocalDateTime)))
      test(s"""{"value": ${ms.removeIfApplicable(",")} }""".read(_.schema("value".instant      ))._assert2(bobj("value" -> instant2      .parseInstant)))

      test(s"""{"value": ${     localTime.quote} }""".read(_.schema("value".     localTime))._assert2(bobj("value" ->      localTime.     parseLocalTime)))
      test(s"""{"value": ${offsetDateTime.quote} }""".read(_.schema("value".offsetDateTime))._assert2(bobj("value" -> offsetDateTime.parseOffsetDateTime)))
      test(s"""{"value": ${ zonedDateTime.quote} }""".read(_.schema("value". zonedDateTime))._assert2(bobj("value" ->  zonedDateTime. parseZonedDateTime))) }

    // ---------------------------------------------------------------------------
    //TODO: binary

    // ---------------------------------------------------------------------------
    test(testing.resourceFilePath("test.json").read().check(bobj("foo" -> "bar1", "baz" -> 1)))
    // TODO: port the rest...
  }

}

// ===========================================================================
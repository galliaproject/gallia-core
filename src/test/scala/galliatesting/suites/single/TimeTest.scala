package galliatesting
package suites
package single

// ===========================================================================
object TimeTest extends utest.TestSuite with GalliaTestSuite { import utest._
  import aptus._ // for parseInstant

  // ---------------------------------------------------------------------------
  val tests = Tests {
    val instant = "2022-03-16T15:38:40Z".parseInstant

    // ---------------------------------------------------------------------------
    test {
      valueWrapper(instant)
          .transform(_.instant("value")).using {
          _.atZone(java.time.ZoneId.systemDefault).getYear }
        ._assert2(gallia.bobj("value" -> 2022))
    }
  }

  // ===========================================================================
  private def valueWrapper[T: WTT](value: T): gallia.domain.AObj = gallia.bobj("value" -> value).forceAObj
}

// ===========================================================================f

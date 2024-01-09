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
      gallia.bobj("value" -> instant).forceAObj
          .transform(_.instant("value")).using {
          _.atZone(java.time.ZoneId.systemDefault).getYear }
        ._assert2(gallia.bobj("value" -> 2022)) } }

}

// ===========================================================================f

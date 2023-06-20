package galliatesting
package suites
package single

// ===========================================================================
object HeadOTest extends utest.TestSuite with GalliaTestSuite {
  import utest._

  // ---------------------------------------------------------------------------
  val tests = Tests {
    test("single union") {
      TestDataO.Default01.convertToMultiple.union(TestDataO.Default01b).check(TestDataS.Default51.forceAObjs)
      TestDataO.Default01.                  union(TestDataO.Default01b).check(TestDataS.Default51.forceAObjs) }
  }
}

// ===========================================================================

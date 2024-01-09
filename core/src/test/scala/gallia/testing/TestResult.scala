package gallia.testing

import gallia.CallSite

// ===========================================================================
case class TestResult(
    suiteName: String, // eg "MergingTest"
    origin   : CallSite,
    value    : TestValue) {

  def location: String =
    origin
      .sub
      .map(_.location)
      .getOrElse("unknown-location")

}
  
// ===========================================================================
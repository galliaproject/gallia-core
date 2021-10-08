package gallia
package inferring.table

import aptus.Anything_

// ===========================================================================
object BooleanDetector {

  // TODO: configurable?
  private val BooleanValues = // case insentive
    Set(
      Set("true", "false"),
      Set("t", "f"),
      Set("0", "1"),
      Set("-1", "1") )

  // ---------------------------------------------------------------------------
  private val FlagValues = // case insentive
    Set(
      "true", "false",
      "t", "f",
      "-1", "0", "1")

  // ===========================================================================
  def isLikelyBoolean(values: Set[String]): Boolean =
    values.map(_.toLowerCase /* likely better heuristic than not */).pipe { y =>
        BooleanValues.exists(_           == y) ||
        FlagValues   .exists(x => Set(x) == y) }

  // ===========================================================================
  def forceBoolean(value: String): Boolean =
      value.toLowerCase match {
        case "true"  | "t" | "1"        => true
        case "false" | "f" | "0" | "-1" => false }

}

// ===========================================================================

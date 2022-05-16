package gallia
package reflect

import aptus.Seq_

// ===========================================================================
object BasicTypeUtils {

  private[reflect] def createLookup(values: Seq[BasicType]): Map[FullName, BasicType] =
    values
      .map { x => x.fullName -> x }
      .force.map
      .withDefault { value => aptus.illegalState(s"TODO:CantFindType:201013093225:${value}") }

  // ===========================================================================
  def normalizeFullName(value: FullName): FullName =
         if (value == "java.lang.Integer") "scala.Int"
    else if (value == "java.lang.String")   value
    else                                    value.replace("java.lang.", "scala.") // not so for java.math (not equivalent at runtime)

  // ===========================================================================
  def combine(values: Seq[BasicType]): BasicType = // TODO: subtype these 3?
    values.distinct.sortBy(_.entryName) match {
      case Seq(                   BasicType._Int                   ) => BasicType._Int
      case Seq(BasicType._Double                                   ) => BasicType._Double
      case Seq(BasicType._Double, BasicType._Int                   ) => BasicType._Double
      case Seq(                                   BasicType._String) => BasicType._String
      case Seq(_                                , BasicType._String) => BasicType._String
      case Seq(_                , _             , BasicType._String) => BasicType._String }

  // ===========================================================================
  private[reflect] def stringOrLong[T](ifString: String => T, ifLong: Long => T)(s: String): T =
    if (!s.forall(_.isDigit)) ifString(s)
    else                      ifLong  (s.toLong)

  // ---------------------------------------------------------------------------
  def doubleFitsFloat(d: Double): Boolean =
    d <= java.lang.Float.MAX_VALUE &&
    d >= java.lang.Float.MIN_VALUE

  // ---------------------------------------------------------------------------
  def doubleFitsLong(d: Double): Boolean =
    d <= java.lang.Long.MAX_VALUE &&
    d >= java.lang.Long.MIN_VALUE

}

// ===========================================================================
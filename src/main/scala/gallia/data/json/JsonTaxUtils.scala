package gallia
package data
package json

import aptus._

// ===========================================================================
object JsonTaxUtils {

  private[json] def stringOrLong[T](ifString: String => T, ifLong: Long => T): Any => T =
    _ match {
      case s: String => ifString(s)
      case n: Number => ifLong  (numberToLong(n)) }

  // ---------------------------------------------------------------------------
  private[json] def stringOrDouble[T](ifString: String => T, ifDouble: Double => T): Any => T =
    _ match {
      case s: String => ifString(s)
      case n: Number => ifDouble(n.doubleValue) }

  // ===========================================================================
  private[json] def numberToLong(n: Number): Long =
    n .doubleValue
      .assert(doubleFitsLong)
      .pipe(d => d.toLong.assert(_.toDouble == d))

  // ---------------------------------------------------------------------------
  private[json] def doubleFitsFloat(d: Double): Boolean =
    d <= java.lang.Float.MAX_VALUE &&
    d >= java.lang.Float.MIN_VALUE

  // ---------------------------------------------------------------------------
  private[json] def doubleFitsLong(d: Double): Boolean =
    d <= java.lang.Long.MAX_VALUE &&
    d >= java.lang.Long.MIN_VALUE
}

// ===========================================================================

package gallia
package data
package json

import aptus._

// ===========================================================================
object JsonTaxUtils {

  private[json] def stringOrLong[T](pair:    (String => T,         Long => T)): Any => T = stringOrLong(pair._1, pair._2)
  private[json] def stringOrLong[T](ifString: String => T, ifLong: Long => T) : Any => T =
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
      .assert(reflect.BasicTypeUtils.doubleFitsLong)
      .pipe(d => d.toLong.assert(_.toDouble == d))

}

// ===========================================================================

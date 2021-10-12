package gallia
package heads.reducing

import aptus.Anything_

import meta._

// ===========================================================================
@NumberAbstraction
@TypeMatching
private object ReducingTypeUtils {

  private[reducing] def _flattenedInts   (values: Values): List[Int   ] = values.flatten.map(_.asInstanceOf[Number].intValue   )
  private[reducing] def _flattenedDoubles(values: Values): List[Double] = values.flatten.map(_.asInstanceOf[Number].doubleValue)

  private[reducing] def _strings(values: Values): List[Option[String]] = values.map(_.map(_.asInstanceOf[String]))

  // ===========================================================================
  def containee(rtipe: ReducingType)(optional: Boolean, originalType: BasicType): Containee =
    if (rtipe == ReducingType.stats)
      originalType match { //FIXME
        case BasicType._String => ReducingStats.Meta.strings     (optional)
        case BasicType._Int    => ReducingStats.Meta.nums[Int   ](optional)
        case BasicType._Double => ReducingStats.Meta.nums[Double](optional)
        case x => ??? }
    else
      rtipe.returnType.newBasicTypeOpt.getOrElse(originalType)

  // ---------------------------------------------------------------------------
  def baseData(f: List[Int   ] => Any, g: List[Double] => Any)(ignored: Boolean, numTypeOpt: Option[NumericalType]): Values => Any =
    numTypeOpt
      .map {
        case BasicType._Int    => x: Values => _flattenedInts   (x).pipe(f)
        case BasicType._Double => x: Values => _flattenedDoubles(x).pipe(g)
        case x => x.p; ??? }
      .getOrElse { ??? }//FIXME: t210118084833 - dates, strings...

  // ---------------------------------------------------------------------------
  def statsData(optional: Boolean, numTypeOpt: Option[NumericalType]) =
    numTypeOpt
      .map {
        case BasicType._Int    => x: Values => ReducingStats.Data.ints   (optional)(x)
        case BasicType._Double => x: Values => ReducingStats.Data.doubles(optional)(x)
        case x => x.p; ??? }
      .getOrElse {
        x: Values => ReducingStats.Data.strings(optional)(x)
        //TODO: dates, ...
      }

}

// ===========================================================================


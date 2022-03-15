package gallia
package heads.reducing

import aptus.Anything_

import meta._

// ===========================================================================
@NumberAbstraction
@TypeMatching
private object ReducingTypeUtils {

  private[reducing] def _flattenedInts       (values: Values): List[Int       ] = values.flatten.map(_.asInstanceOf[Number].intValue   )
  private[reducing] def _flattenedDoubles    (values: Values): List[Double    ] = values.flatten.map(_.asInstanceOf[Number].doubleValue)    
  
  private[reducing] def _flattenedBytes      (values: Values): List[Byte      ] = values.flatten.map(_.asInstanceOf[Number].byteValue  )
  private[reducing] def _flattenedShorts     (values: Values): List[Short     ] = values.flatten.map(_.asInstanceOf[Number].shortValue )
  private[reducing] def _flattenedLongs      (values: Values): List[Long      ] = values.flatten.map(_.asInstanceOf[Number].longValue  )
  
  private[reducing] def _flattenedFloats     (values: Values): List[Float     ] = values.flatten.map(_.asInstanceOf[Number].floatValue )
  
  private[reducing] def _flattenedBigInts    (values: Values): List[BigInt    ] = values.flatten.map(_.asInstanceOf[BigInt]    )
  private[reducing] def _flattenedBigDecimals(values: Values): List[BigDecimal] = values.flatten.map(_.asInstanceOf[BigDecimal])

  // ---------------------------------------------------------------------------
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
  def baseData
      (ignored: Boolean, numTypeOpt: Option[NumericalType])
      ( ints       : List[Int]        => Any,          
        doubles    : List[Double]     => Any,

        bytes      : List[Byte]       => Any,
        shorts     : List[Short]      => Any,
        longs      : List[Long]       => Any,

        floats     : List[Float]      => Any,

        bigInts    : List[BigInt]     => Any,
        bigDecimals: List[BigDecimal] => Any)
      : Values => Any =
    numTypeOpt
      .map {
        case BasicType._Int    => (x: Values) => _flattenedInts   (x).pipe(ints)
        case BasicType._Double => (x: Values) => _flattenedDoubles(x).pipe(doubles)
        
        case BasicType._Byte   => (x: Values) => _flattenedBytes  (x).pipe(bytes)
        case BasicType._Short  => (x: Values) => _flattenedShorts (x).pipe(shorts)
        case BasicType._Long   => (x: Values) => _flattenedLongs  (x).pipe(longs)
        case BasicType._Float  => (x: Values) => _flattenedFloats (x).pipe(floats)        

        case BasicType._BigInt => (x: Values) => _flattenedBigInts    (x).pipe(bigInts)        
        case BasicType._BigDec => (x: Values) => _flattenedBigDecimals(x).pipe(bigDecimals) }
      .getOrElse { ??? }//FIXME: t210118084833 - dates, strings...

  // ---------------------------------------------------------------------------
  def statsData(optional: Boolean, numTypeOpt: Option[NumericalType]) =
    numTypeOpt
      .map {
        case BasicType._Int    => (x: Values) => ReducingStats.Data.ints   (optional)(x)
        case BasicType._Double => (x: Values) => ReducingStats.Data.doubles(optional)(x)
        case x => x.p; ??? }
      .getOrElse {
        (x: Values) => ReducingStats.Data.strings(optional)(x)
        //TODO: dates, ...
      }

}

// ===========================================================================


package gallia
package heads
package reducing

import aptus.Anything_

import meta._

// ===========================================================================
@NumberAbstraction
@TypeMatching
private object ReducingTypeUtils {
  import ReducingStats.{Meta, Data}
  
  // ===========================================================================
  private[reducing] def _flattenedAsDoubles(values: Values): List[Double] = values.flatten.map(_.asInstanceOf[Number].doubleValue)

  // ---------------------------------------------------------------------------
  private[reducing] def _flattenedInts   (values: Values): List[Int   ] = values.flatten.asInstanceOf[List[Int   ]]
  private[reducing] def _flattenedDoubles(values: Values): List[Double] = values.flatten.asInstanceOf[List[Double]]    
  
  private[reducing] def _flattenedBytes  (values: Values): List[Byte  ] = values.flatten.asInstanceOf[List[Byte  ]]
  private[reducing] def _flattenedShorts (values: Values): List[Short ] = values.flatten.asInstanceOf[List[Short ]]
  private[reducing] def _flattenedLongs  (values: Values): List[Long  ] = values.flatten.asInstanceOf[List[Long  ]]
  
  private[reducing] def _flattenedFloats (values: Values): List[Float ] = values.flatten.asInstanceOf[List[Float ]]

  private[reducing] def _flattenedBigInts(values: Values): List[BigInt] = values.flatten.asInstanceOf[List[BigInt]]
  private[reducing] def _flattenedBigDecs(values: Values): List[BigDec] = values.flatten.asInstanceOf[List[BigDec]]

  // ---------------------------------------------------------------------------
  private[reducing] def _strings(values: Values): List[Option[String]] = values.asInstanceOf[List[Option[String]]]

  // ===========================================================================
  def valueType(rtipe: ReducingType)(optional: Boolean, originalType: BasicType): ValueType =
      if (rtipe == ReducingType.stats) stats(optional, originalType)
      else                             rtipe.returnType.newBasicTypeOpt.getOrElse(originalType)
  
    // ---------------------------------------------------------------------------    
    private def stats(optional: Boolean, originalType: BasicType): ValueType =
        originalType match { //FIXME
          case BasicType._String => Meta.strings     (optional)
          
          // ---------------------------------------------------------------------------
          case BasicType._Int    => Meta.nums[Int   ](optional)
          case BasicType._Double => Meta.nums[Double](optional)
          
          case BasicType._Byte   => Meta.nums[Byte]  (optional)
          case BasicType._Short  => Meta.nums[Short] (optional)
          case BasicType._Long   => Meta.nums[Long]  (optional)
          case BasicType._Float  => Meta.nums[Float] (optional)
          
          case BasicType._BigInt => Meta.nums[BigInt](optional)
          case BasicType._BigDec => Meta.nums[BigDec](optional)
  
          case x => ??? }

  // ---------------------------------------------------------------------------
  def baseData
      (ignored: Boolean, numTypeOpt: Option[NumericalType])
      ( ints   : List[Int]    => Any,          
        doubles: List[Double] => Any,

        bytes  : List[Byte]   => Any,
        shorts : List[Short]  => Any,
        longs  : List[Long]   => Any,

        floats : List[Float]  => Any,

        bigInts: List[BigInt] => Any,
        bigDecs: List[BigDec] => Any)
      : Values => Any =
    numTypeOpt
      .map {
        case BasicType._Int    => (x: Values) => _flattenedInts   (x).pipe(ints)
        case BasicType._Double => (x: Values) => _flattenedDoubles(x).pipe(doubles)
        
        case BasicType._Byte   => (x: Values) => _flattenedBytes  (x).pipe(bytes)
        case BasicType._Short  => (x: Values) => _flattenedShorts (x).pipe(shorts)
        case BasicType._Long   => (x: Values) => _flattenedLongs  (x).pipe(longs)
        case BasicType._Float  => (x: Values) => _flattenedFloats (x).pipe(floats)        

        case BasicType._BigInt => (x: Values) => _flattenedBigInts(x).pipe(bigInts)        
        case BasicType._BigDec => (x: Values) => _flattenedBigDecs(x).pipe(bigDecs) }
      .getOrElse { ??? }//FIXME: t210118084833 - dates, strings...

  // ---------------------------------------------------------------------------
  def statsData(optional: Boolean, numTypeOpt: Option[NumericalType]):  Values => Obj =
    numTypeOpt
      .map {
        case BasicType._Int    => (x: Values) => Data.ints   (optional)(x)
        case BasicType._Double => (x: Values) => Data.doubles(optional)(x)
        
        case BasicType._Byte   => (x: Values) => Data.bytes  (optional)(x)
        case BasicType._Short  => (x: Values) => Data.shorts (optional)(x)
        case BasicType._Long   => (x: Values) => Data.longs  (optional)(x)
        case BasicType._Float  => (x: Values) => Data.floats (optional)(x)
        
        case BasicType._BigInt => (x: Values) => Data.bigInts(optional)(x)
        case BasicType._BigDec => (x: Values) => Data.bigDecs(optional)(x)
        
        case x => x.p; ??? }
      .getOrElse {
        (x: Values) => Data.strings(optional)(x)
        //TODO: dates (t220330131813), ...
      }

}

// ===========================================================================


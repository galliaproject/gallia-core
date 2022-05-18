package gallia
package heads.reducing

import aptus.{String_, Seq_}

import meta._
import reflect.Container

// ===========================================================================
sealed trait ReducingType { // don't make enum (else can't use `values`)
    def pair(u: Key) = ReducingPair1(u, this)

    // ---------------------------------------------------------------------------
    final def field(          original: Fld): Fld = field(defaultKey, original)
    final def field(key: Key, original: Fld): Fld = Fld(key, container.info(valueType(original.isOptional, original.forceBasicType)))

      // ---------------------------------------------------------------------------
      protected def container: Container = Container._One // only overriden by no grouping (remains Seq)

      // ---------------------------------------------------------------------------
      protected def valueType(optional: Boolean, originalType: BasicType): ValueType =
        ReducingTypeUtils.valueType(this)(optional, originalType)

    // ---------------------------------------------------------------------------
    val defaultKey      : Key
    def defaultPluralKey: Key = s"${defaultKey.name}s".symbol

    def returnType: ReduceReturnType
    def data(optional: Boolean, numTypeOpt: Option[NumericalType]): Values => AnyValue
  }

  // ===========================================================================
  object ReducingType { // don't make enum (else can't use `values`)

    protected sealed trait Base[T] extends ReducingType {
        final def data(ignored1: Boolean, ignored2: Option[NumericalType]) = data
        def data: Values => T
      }

    // ---------------------------------------------------------------------------
    @deprecated("use CountLikeType")
    protected[reducing] sealed trait CountAgg extends Base[Int] {
        val defaultKey = _count_all
        def returnType = ReduceReturnType.integer }

    // ---------------------------------------------------------------------------
    protected abstract class RealAgg(val defaultKey: Key) extends Base[Double] { def returnType = ReduceReturnType.real }

    // ---------------------------------------------------------------------------
    protected[reducing] abstract class _Base( // TODO: rename
            val defaultKey: Key,
            val returnType: ReduceReturnType,

            // ---------------------------------------------------------------------------
            val ints       : List[Int   ] => Any,          
            val doubles    : List[Double] => Any,
                        
            val bytes      : List[Byte]   => Any,
            val shorts     : List[Short]  => Any,
            val longs      : List[Long]   => Any,
            
            val floats     : List[Float]  => Any,
                        
            val bigInts    : List[BigInt] => Any,
            val bigDecimals: List[BigDec] => Any)
          extends ReducingType {
        def data(ignored: Boolean, numTypeOpt: Option[NumericalType]): Values => Any =
          ReducingTypeUtils.baseData(ignored, numTypeOpt)(
              ints       ,
              doubles    ,

              bytes      ,
              shorts     ,
              longs      ,
                          
              floats     ,
                                      
              bigInts    ,
              bigDecimals)            
      }

    // ===========================================================================
    //TODO:
    // - t210118084355 - mode, skewness, curtosis, MAD/mean absolute deviation, trimmed ...
    // - t210118084356 - distributions? (pretty costly)?

    case object count_all              extends CountLikeType { def data = countAll             _ }
    case object count_present          extends CountLikeType { def data = countPresent         _ }
    case object count_missing          extends CountLikeType { def data = countMissing         _ }
    case object count_distinct         extends CountLikeType { def data = countDistinct        _ }
    case object count_distinct_present extends CountLikeType { def data = countDistinctPresent _ }

    // ---------------------------------------------------------------------------
    // FIXME: t220330111011 - bignums...
    case object mean   extends RealAgg(_mean  ) { def data = ReducingTypeUtils._flattenedAsDoubles(_).mean  }
    case object stdev  extends RealAgg(_stdev ) { def data = ReducingTypeUtils._flattenedAsDoubles(_).stdev }
    //TODO: variance?

    case object median extends RealAgg(_median) { def data = ReducingTypeUtils._flattenedAsDoubles(_).median } //TODO: t201207134039 - median floor/ceiling for ints (or left/right?)
    // TODO: percentile(n), quantile

    // ===========================================================================
    case object sum   extends _Base(_sum  , ReduceReturnType.unchanged, _.sum, _.sum, _.sum, _.sum, _.sum, _.sum, _.sum, _.sum)

    // ---------------------------------------------------------------------------
    case object min   extends _Base(_min  , ReduceReturnType.unchanged, _.min  , _.min, _.min  , _.min, _.min, _.min, _.min, _.min)
    case object max   extends _Base(_max  , ReduceReturnType.unchanged, _.max  , _.max, _.max  , _.max, _.max, _.max, _.max, _.max)
    case object range extends _Base(_range, ReduceReturnType.unchanged, _.range, _.range, _.range, _.range, _.range, _.range, _.range, _.range)

    // ---------------------------------------------------------------------------
    case object IQR   extends _Base(_IQR  , ReduceReturnType.real     , _.IQR  , _.IQR, _.IQR, _.IQR, _.IQR, _.IQR, _.IQR, _.IQR  )

    // ===========================================================================
    case object stats extends ReducingType {
      val defaultKey = _stats
      def returnType = null// TODO: N/A - relates to t210115144940?

      // ---------------------------------------------------------------------------
      def data(optional: Boolean, numTypeOpt: Option[NumericalType]) =
        ReducingTypeUtils.statsData(optional, numTypeOpt)
    }

    // ===========================================================================
    case object values /* = no aggregation (just flattening) */ extends ReducingType {
      val defaultKey = _values
      override protected def container = Container._Nes
      def returnType = ReduceReturnType.unchanged
      def data(ignored1: Boolean, ignored2: Option[NumericalType]) = _.flatten
    }

    // ===========================================================================
    private def countAll              (values: Values): Int = values                             .size
      private def countPresent        (values: Values): Int = values.filter(_.isDefined)         .size
      private def countMissing        (values: Values): Int = values.filter(_.isEmpty  )         .size
      private def countDistinct       (values: Values): Int = values                    .distinct.size
      private def countDistinctPresent(values: Values): Int = values.filter(_.isDefined).distinct.size
  }

// ===========================================================================
sealed trait CountLikeType extends ReducingType.CountAgg

  object CountLikeType {
    val count_all              = ReducingType.count_all    
    val count_present          = ReducingType.count_present
    val count_missing          = ReducingType.count_missing
    val count_distinct         = ReducingType.count_distinct
    val count_distinct_present = ReducingType.count_distinct_present
  }

// ===========================================================================


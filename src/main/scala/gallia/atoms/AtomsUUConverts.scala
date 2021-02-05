package gallia.atoms

import aptus.String_

import gallia._
import gallia.domain._

// ===========================================================================
object AtomsUUConverts {
  import AtomsUUTransforms._TransformVV

  // ===========================================================================
  case class _ConvertToString(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, _.toString).naive(o) }

    // ===========================================================================
    case class _ConvertToInt(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toInt).naive(o)

      // ---------------------------------------------------------------------------
      private def toInt(value: Any): Any =
        util.Try {
          utils.AtomsUtils.applyx {
              case x: String => x.stripTrailingZeros.toInt
              case x: Number => x.intValue()
            } (value) }
        .getOrElse{
          runtimeError(s"TODO:210106152700:${target}:${value}") }
    }

    // ===========================================================================
    case class _ConvertToDouble(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toDouble).naive(o)

      // ---------------------------------------------------------------------------
      private def toDouble(value: Any): Any =
        util.Try {
          utils.AtomsUtils.applyx {
              case x: String => x.toDouble
              case x: Number => x.doubleValue
            }(value) }
        .getOrElse{
          runtimeError(s"TODO:210106152701:${target}:${value}") }
    }

  // ===========================================================================
  case class _ConvertToFlag(target: PathPair, trueValue: Any, strict: Boolean) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toFlag(trueValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toFlag(trueValue: Any) = (value: Any) =>
        if (target.matching(value, trueValue)) true
        else
               if (value == None) None
          else if (strict)        runtimeError(s"TODO:210108150541:${target}:${value}")
          else                    None }

    // ===========================================================================
    case class _ConvertToBoolean(target: PathPair, trueValue: Any, falseValue: Any) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toBoolean(trueValue, falseValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toBoolean(trueValue: Any, falseValue: Any) = (value: Any) =>
               if (target.matching(value, trueValue )) true
          else if (target.matching(value, falseValue)) false
          else                                         throw new IllegalArgumentException(s"TODO:210108093025:${value}:${trueValue}:${falseValue}") }

    // ===========================================================================
    case class _ConvertToOptionalBoolean(target: PathPair, trueValue: Any, falseValue: Any, nullValue: Any) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toOptionalBoolean(trueValue, falseValue, nullValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toOptionalBoolean(trueValue: Any, falseValue: Any, nullValue: Any) =
        (value: Any) =>
               if (target.matching(value, trueValue )) true
          else if (target.matching(value, falseValue)) false
          else if (target.matching(value, nullValue )) None
          else                                         throw new IllegalArgumentException(s"TODO:210108093026:${value}:${trueValue}:${falseValue}:${nullValue}") }

}

// ===========================================================================


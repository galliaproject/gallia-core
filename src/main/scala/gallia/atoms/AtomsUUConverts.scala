package gallia
package atoms

import aptus.String_

import domain._

// ===========================================================================
object AtomsUUConverts {
  import AtomsUUTransforms._TransformVV

  // ===========================================================================
  case class _ConvertToString(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toStr).naive(o)

      // ---------------------------------------------------------------------------
      private def toStr(value: Any) =
        util.Try {
          utils.AtomsUtils.applyx(_.toString)(value) }
        .getOrElse {
          dataError(s"TODO:210106152659:${target}:${value}") }
    }

    // ===========================================================================
    case class _ConvertToInt(origin: CallSite)(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toInt).naive(o)

      // ---------------------------------------------------------------------------
      private def toInt(value: Any): Any =
        util.Try {
          utils.AtomsUtils.applyx {
              case x: String => x.stripTrailingZeros.toInt
              case x: Number => x.intValue()
            } (value) }
        .getOrElse {
          dataError(s"TODO:210106152700:${target}:${value}:${origin}") }
    }

    // ===========================================================================
    case class _ConvertToDouble(origin: CallSite)(target: PathPair) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toDouble).naive(o)

      // ---------------------------------------------------------------------------
      private def toDouble(value: Any): Any =
        util.Try {
          utils.AtomsUtils.applyx {
              case x: String => x.toDouble
              case x: Number => x.doubleValue
            }(value) }
        .getOrElse{
          dataError(s"TODO:210106152701:${target}:${value}:${origin}") }
    }

  // ===========================================================================
  case class _ConvertToFlag(origin: CallSite)(target: PathPair, trueValue: Any, strict: Boolean) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toFlag(trueValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toFlag(trueValue: Any) = { (value: Any) =>
        if (target.matching(value, trueValue)) true
        else {
               if (value == None) None
          else if (strict)        dataError(s"TODO:210108150541:${target}:${value}:${origin}")
          else                    None } } }

    // ===========================================================================
    case class _ConvertToBoolean(origin: CallSite)(target: PathPair, trueValue: Any, falseValue: Any) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toBoolean(trueValue, falseValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toBoolean(trueValue: Any, falseValue: Any) = { (value: Any) =>
               if (target.matching(value, trueValue )) true
          else if (target.matching(value, falseValue)) false
          else                                         dataError(s"TODO:210108093025:${value}:${trueValue}:${falseValue}:${origin}") } }

    // ===========================================================================
    case class _ConvertToOptionalBoolean(origin: CallSite)(target: PathPair, trueValue: Any, falseValue: Any, nullValue: Any) extends AtomUU { def naive(o: Obj) =
        _TransformVV(target, toOptionalBoolean(trueValue, falseValue, nullValue)).naive(o)

      // ---------------------------------------------------------------------------
      private def toOptionalBoolean(trueValue: Any, falseValue: Any, nullValue: Any) = { (value: Any) =>
             if (target.matching(value, trueValue )) true
        else if (target.matching(value, falseValue)) false
        else if (target.matching(value, nullValue )) None
        else                                         dataError(s"TODO:210108093026:${value}:${trueValue}:${falseValue}:${nullValue}:${origin}") } }

}

// ===========================================================================


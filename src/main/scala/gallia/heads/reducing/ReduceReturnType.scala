package gallia
package heads.reducing

import enumeratum.{Enum, EnumEntry}

// ===========================================================================
@gallia.NumberAbstraction
sealed trait ReduceReturnType extends EnumEntry { // 210123095857

  def newBasicTypeOpt: Option[BasicType] = this match {
    case ReduceReturnType.unchanged => None
    case ReduceReturnType.integer   => Some(BasicType._Int)
    case ReduceReturnType.real      => Some(BasicType._Double) } }

  // ---------------------------------------------------------------------------
  object ReduceReturnType
      extends         Enum[ReduceReturnType] {
    val values = findValues

    case object unchanged extends ReduceReturnType
    case object integer   extends ReduceReturnType
    case object real      extends ReduceReturnType

    /*
      match {
        case ReduceReturnType.unchanged => ???
        case ReduceReturnType.integer   => ???
        case ReduceReturnType.real      => ???
      }
     */
  }


// ===========================================================================

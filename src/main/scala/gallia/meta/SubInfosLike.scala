package gallia
package meta

import aptus.Seq_

// ===========================================================================
trait SubInfosLike extends HasValueTypes {
                 def union     : Seq[SubInfo]
  final override def valueTypes: Seq[ValueType] = union.map(_.valueType)

  // ===========================================================================
  def forceSubInfo: SubInfo = union.force.one // there are legitimate cases too

  // ---------------------------------------------------------------------------
  @deprecated def subInfo1: SubInfo = union match {  // see t210125111338 (union types)
    case Seq(sole) => sole
    case _         => aptus.unsupportedOperation("limited support for union types (see t210125111338)") }

  // ===========================================================================
  def isUnionType: Boolean = union.size > 1 // see t210125111338 (union types)

  def nonUnionTypeSubInfoOpt: Option[SubInfo] = if (isUnionType) None else Some(union.force.one)

  // ---------------------------------------------------------------------------
  def hasMultiple: Boolean = union.exists( _.multiple)
  def hasSingle  : Boolean = union.exists(!_.multiple)

  // ---------------------------------------------------------------------------
  def areAllMultiple: Boolean = union.forall( _.multiple)
  def areAllSingle  : Boolean = union.forall(!_.multiple)
}

// ===========================================================================

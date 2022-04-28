package gallia
package meta

import aptus.Seq_

// ===========================================================================
trait InfosLike extends HasContainees {
                 def infos     : Seq[Info]
  final override def containees: Seq[Containee] = infos.map(_.containee)

  // ===========================================================================
  def forceInfo: Info = infos.force.one // there are legitimate cases too

  // ---------------------------------------------------------------------------
  @deprecated def info1: Info = infos match {  // see t210125111338 (union types)
    case Seq(sole) => sole
    case more      => aptus.unsupportedOperation("limited support for union types (see t210125111338)") }

  // ===========================================================================
  def isUnionType: Boolean = infos.size > 1 // see t210125111338 (union types)

  def nonUnionTypeInfoOpt: Option[Info] = if (isUnionType) None else Some(infos.force.one)

  // ---------------------------------------------------------------------------
  def hasMultiple: Boolean = infos.exists( _.multiple)
  def hasSingle  : Boolean = infos.exists(!_.multiple)

  // ---------------------------------------------------------------------------
  def areAllMultiple: Boolean = infos.forall( _.multiple)
  def areAllSingle  : Boolean = infos.forall(!_.multiple)
}

// ===========================================================================

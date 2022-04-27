package gallia
package meta

import aptus.Seq_

// ===========================================================================
trait InfosLike extends HasContainees {
                 protected      val infos      : Seq[Info]
  final override protected lazy val _containees: Seq[Containee] = infos.map(_.containee)

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

package gallia
package actions

import aptus.Anything_

import heads.merging.MergingData
import heads.merging.MergingData._
import atoms.AtomsZZMerging._

// ===========================================================================
object ActionsZZMerging {

  case object UnionZ extends ActionZzToZ {
    def _meta (c1: Cls , c2: Cls ): Cls  = c1.unionCompatible(c2) //TODO: val
    def dataz2(c1: Cls , c2: Cls ): Atoms = _UnionZZ.in.seq }

  // ---------------------------------------------------------------------------
  case object ZipZ extends ActionZzToZ {
    def _meta (c1: Cls , c2: Cls ): Cls  = c1.merge(c2) //TODO: val
    def dataz2(c1: Cls , c2: Cls ): Atoms = _ZipZZ.in.seq }

  // ===========================================================================
  case class MergeObjsVle(f: (Objs, Vle) => Objs) extends ActionZvToZ {
    def _meta (c1: Cls , c2: Cls): Cls  = c1 // TODO: t220718112411
    def dataz2(c1: Cls , c2: Cls): Atoms = _MergeObjsVle(f).in.seq }

  // ===========================================================================
  case class Merging(data: MergingData) extends ActionZzToZ {
    override def vldt (c1: Cls, c2: Cls): Errs = {
      data
        .vldtJoinKeys(c1, c2).map(_Error.AmbiguousMergingKey).map(_.err).toSeq
        .orIfEmpty {
          val jk = data.joinKeys(c1, c2)       
          val xs = c1.keys.intersect(c2.keys).diff(jk.keys)

          if (xs.nonEmpty) Seq(_Error.NameConflictsForJoin(Keyz(xs)).err) // t220209085836 - name conflict in join: offer mode to discard RHS conflicts, and mode to rename RHS
          else             Nil        
        }
    }

    // ---------------------------------------------------------------------------
    def _meta(c1: Cls , c2: Cls): Cls =
      data match {
        case bring: BringData    => c1.bring(c2, bring.targetKeys(c1, c2))

        // do not need join type
        case join   : JoinData    => c1.join   (c2)(join   .joinType, join   .joinKeys(c1, c2))
        case coGroup: CoGroupData => c1.coGroup(c2)(coGroup.joinType, coGroup.joinKeys(c1, c2), coGroup.as) }

    // ---------------------------------------------------------------------------
    def dataz2(c1: Cls , c2: Cls): Atoms =
      data
        .joinKeys(c1, c2)
        .pipe { joinKeys =>
          data match {
            case bring  : BringData   => _Bring  (bring.targetKeys(c1, c2), joinKeys)
            case join   : JoinData    => _Join   (join   .joinType        , joinKeys)
            case coGroup: CoGroupData => _CoGroup(coGroup.joinType        , joinKeys, coGroup.as) } }
        .in.seq }

}

// ===========================================================================


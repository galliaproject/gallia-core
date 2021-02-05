package gallia.meta

import aptus.Anything_

import gallia._
import gallia.heads.merging.MergingData._

// ===========================================================================
trait ClsMerging { self: Cls =>

  def bring(that: Cls, keys: Keyz): Cls =
    that
      .retain(keys /* already excludes join key if was selected */)
      .mapFields(_.toNonRequired)
      .thn(mergeDisjoint)

  // ---------------------------------------------------------------------------
  def coGroup(that: Cls)(joinKeys: JoinKey, as: AsKeys): Cls =
    Cls(
      field(joinKeys.left) +: // arbitrarily choosing left as the "dominant" one
      Seq(
        Fld(as.left , Info.pes(this.remove(joinKeys.left ))),
        Fld(as.right, Info.pes(that.remove(joinKeys.right))) ) )

  // ---------------------------------------------------------------------------
  def join(that: Cls)(joinKeys: JoinKey): Cls =
    Cls(
      this                       .mapFields(_.thnIf(_.key != joinKeys.left)(_.toNonRequired)).fields ++
      that.remove(joinKeys.right).mapFields(                                _.toNonRequired ).fields)

}

// ===========================================================================

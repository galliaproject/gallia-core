package gallia
package meta

import aptus.{Anything_, Seq_}

import heads.merging.MergingData._

// ===========================================================================
trait ClsMerging { self: Cls =>

  def bring(that: Cls, keys: Keyz): Cls =
    that
      .retain(keys /* already excludes join key if was selected */)
      .mapFields(_.toOptional) // TODO: feature: t201124153838, bring "guaranteed" 
      .pipe(mergeDisjoint)

  // ---------------------------------------------------------------------------
  def coGroup(that: Cls)(joinType: JoinType, joinKeys: JoinKey, as: AsKeys): Cls =
      Cls(
        field(joinKeys.left) +: // arbitrarily choosing left as the "dominant" one
        Seq(
          Fld(as.left , Info(coGroupOptionality(joinType, isLeft = true) , SubInfo.multiple(this.remove(joinKeys.left )))),
          Fld(as.right, Info(coGroupOptionality(joinType, isLeft = false), SubInfo.multiple(that.remove(joinKeys.right)))) ) )

    // ---------------------------------------------------------------------------
    private def coGroupOptionality(joinType: JoinType, isLeft: Boolean): Optional =
      joinType match {
        case JoinType.full  =>                             _Optional
        case JoinType.left  => if ( isLeft) _Required else _Optional
        case JoinType.right => if (!isLeft) _Required else _Optional
        case JoinType.inner =>              _Required }

  // ---------------------------------------------------------------------------
  def join(that: Cls)(joinType: JoinType, joinKeys: JoinKey): Cls = {
    def thisFields = 
      this
        .mapFields(_.pipeIf(_.key != joinKeys.left)(_.toOptional))
        .fields

    def thatFields(b: Boolean): Seq[Fld] =
      that
        .fields
        .filterNot(_.key == joinKeys.right)
        .mapIf(_ => b)(_.toOptional)
    
    joinType match {
      case JoinType.full  => Cls(thisFields  ++ thatFields(true) )                        
      case JoinType.left  => Cls(self.fields ++ thatFields(true) )                        
      case JoinType.right => Cls(thisFields  ++ thatFields(false))
      case JoinType.inner => Cls(self.fields ++ thatFields(false)) }
  }

}

// ===========================================================================

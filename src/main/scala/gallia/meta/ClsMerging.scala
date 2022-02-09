package gallia
package meta

import aptus.{Anything_, Seq_}

import heads.merging.MergingData._

// ===========================================================================
trait ClsMerging { self: Cls =>

  def bring(that: Cls, keys: Keyz): Cls =
    that
      .retain(keys /* already excludes join key if was selected */)
      .mapFields(_.toNonRequired) // TODO: feature: t201124153838, bring "guaranteed" 
      .pipe(mergeDisjoint)

  // ---------------------------------------------------------------------------
  def coGroup(that: Cls)(joinType: JoinType, joinKeys: JoinKey, as: AsKeys): Cls =
      Cls(
        field(joinKeys.left) +: // arbitrarily choosing left as the "dominant" one
        Seq(
          Fld(as.left , Info(coGroupContainer(joinType, isLeft = true) , this.remove(joinKeys.left ))),
          Fld(as.right, Info(coGroupContainer(joinType, isLeft = false), that.remove(joinKeys.right))) ) )
  
    // ---------------------------------------------------------------------------
    private def coGroupContainer(joinType: JoinType, isLeft: Boolean): Container =
      joinType match {
        case JoinType.full  => Container._Pes                    
        case JoinType.left  => if (isLeft) Container._Nes else Container._Pes
        case JoinType.right => if (isLeft) Container._Pes else Container._Nes
        case JoinType.inner => Container._Nes }
  
  // ---------------------------------------------------------------------------
  def join(that: Cls)(joinType: JoinType, joinKeys: JoinKey): Cls = {
    def thisFields = 
      this
        .mapFields(_.pipeIf(_.key != joinKeys.left)(_.toNonRequired))
        .fields

    def thatFields(b: Boolean): Seq[Fld] =
      that
        .fields
        .filterNot(_.key == joinKeys.right)
        .mapIf(_ => b)(_.toNonRequired)
    
    joinType match {
      case JoinType.full  => Cls(thisFields  ++ thatFields(true) )                        
      case JoinType.left  => Cls(self.fields ++ thatFields(true) )                        
      case JoinType.right => Cls(thisFields  ++ thatFields(false))
      case JoinType.inner => Cls(self.fields ++ thatFields(false)) }
  }

}

// ===========================================================================

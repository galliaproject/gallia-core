package gallia.meta

import aptus.Anything_

import gallia._
import gallia.heads.merging.MergingData._

// ===========================================================================
trait ClsMerging { self: Cls =>

  def bring(that: Cls, keys: Keyz): Cls =
    that
      .retain(keys /* already excludes join key if was selected */)
      .mapFields(_.toNonRequired) // TODO: feature: t201124153838, bring "guaranteed" 
      .thn(mergeDisjoint)

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
    val that2 = that.remove(joinKeys.right)
    
    joinType match {
      case JoinType.full  => Cls(this.mapFields(_.thnIf(_.key != joinKeys.left)(_.toNonRequired)).fields ++ that2.mapFields(_.toNonRequired ).fields)                        
      case JoinType.left  => Cls(this.fields                                                             ++ that2.mapFields(_.toNonRequired ).fields)                        
      case JoinType.right => Cls(this.mapFields(_.thnIf(_.key != joinKeys.left)(_.toNonRequired)).fields ++ that2.fields)
      case JoinType.inner => Cls(this.fields                                                             ++ that2.fields) }
  }

}

// ===========================================================================

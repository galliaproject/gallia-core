package gallia
package domain

// ===========================================================================
sealed trait GroupingPair

  // ---------------------------------------------------------------------------
  object GroupingPair {

    case class GroupingPair11(grouper : Fld, groupee : Fld) extends GroupingPair
    case class GroupingPairN1(groupers: Cls, groupee : Fld) extends GroupingPair
    case class GroupingPair1N(grouper : Fld, groupees: Cls) extends GroupingPair
    case class GroupingPairNN(groupers: Cls, groupees: Cls) extends GroupingPair

    // ---------------------------------------------------------------------------
    object GroupingPair11 { def from(c: Cls)(groupee : Ren , grouper : Ren)  = GroupingPair11(c.field (grouper) , c.field (groupee )) -> (groupee , grouper ) }
    object GroupingPairN1 { def from(c: Cls)(groupee : Ren , groupers: Renz) = GroupingPairN1(c.retain(groupers), c.field (groupee )) -> (groupee , groupers) }
    object GroupingPair1N { def from(c: Cls)(groupees: Renz, grouper : Ren ) = GroupingPair1N(c.field (grouper) , c.retain(groupees)) -> (groupees, grouper ) }
    object GroupingPairNN { def from(c: Cls)(groupees: Renz, groupers: Renz) = GroupingPairNN(c.retain(groupers), c.retain(groupees)) -> (groupees, groupers) }
  }

// ===========================================================================
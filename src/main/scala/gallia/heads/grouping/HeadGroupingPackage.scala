package gallia
package heads

// ===========================================================================
package object grouping extends _heads {

  type Groupee1Selection = SEL.Groupee1.Selector
  type GroupeeNSelection = SEL.GroupeeN.Selector
  type GroupersSelection = SEL.Groupers.Selector

  // ---------------------------------------------------------------------------
  val resolveGroupee  = SEL.Groupee1.resolve _
  val resolveGroupees = SEL.GroupeeN.resolve _
  val resolveGroupers = SEL.Groupers.resolve _

  // ===========================================================================
  type ReducingType = gallia.heads.reducing.ReducingType
  val  ReducingType = gallia.heads.reducing.ReducingType

  // ===========================================================================
  trait HasAs { self: HeadZ => // TODO: t210116192032 - generalize
    def asDefault            : HeadZ = self
    def as(newKey: gallia.KeyW): HeadZ = self.updateAs1(newKey.value) }

}

// ===========================================================================
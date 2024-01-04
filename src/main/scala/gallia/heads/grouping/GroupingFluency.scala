package gallia
package heads
package grouping

import trgt._

// ===========================================================================
object GroupingFluency { // TODO: DWH/MDX: t210124100722
  import GroupData._

  // ---------------------------------------------------------------------------
  class Start {
      def all: AllBy = new AllBy()

      // ---------------------------------------------------------------------------
      def field(target: Ren)              : By1 = field(_.explicit(target))
      def field(target: Groupee1Selection): By1 = new By1(resolveGroupee(target))

      // ---------------------------------------------------------------------------
      def fields(targets: Renz)             : ByN = fields(_.explicit(targets))
      def fields(targets: GroupeeNSelection): ByN = new ByN(resolveGroupees(targets))

      // ---------------------------------------------------------------------------
      private[heads] def fields(targets: TqRenz): ByN = new ByN(targets)
    }

    // ===========================================================================
    class AllBy() {
        def by(groupers: RenWz)            : End with GroupAs = by(_.explicit(groupers))
        def by(grouper1: RenW, more: RenW*): End with GroupAs = by(_.explicitFX(grouper1, more))
        def by(groupers: GroupersSelection): End with GroupAs = new End(GroupDataCN(resolveGroupers(groupers), asOpt = None)) with GroupAs }

      // ===========================================================================
      class By1(groupee: TqRen) {
        def by(groupers: RenWz)            : End = by(_.explicit(groupers))
        def by(grouper1: RenW, more: RenW*): End = by(_.explicitFX(grouper1, more))
        def by(groupers: GroupersSelection): End = new End(GroupData1N(groupee, resolveGroupers(groupers)))

        private[heads] def byN(groupers: TqRenz): End = new End(GroupData1N(groupee, groupers))

        // ---------------------------------------------------------------------------
        def byTheRest: End = new End(GroupData1C(groupee)) }

      // ===========================================================================
      class ByN(groupees: TqRenz) {
        def by(groupers: RenWz)            : End with GroupAs = by(_.explicit(groupers))
        def by(grouper1: RenW, more: RenW*): End with GroupAs = by(_.explicitFX(grouper1, more))
        def by(groupers: GroupersSelection): End with GroupAs = new End(GroupDataNN(groupees, resolveGroupers(groupers), asOpt = None)) with GroupAs

        // ---------------------------------------------------------------------------
        def byTheRest: End with GroupAs = new End(GroupDataNC(groupees, asOpt = None)) with GroupAs }

      // ===========================================================================
      trait GroupAs { self: End => // similar to HasAs; TODO: t210116192032 - generalize
          def asDefault       : End = as(_group)
          def as(target: KeyW): End = new End(self.data.forceAs(target.value)) }

        // ===========================================================================
        class End(val data: GroupData)
}

// ===========================================================================

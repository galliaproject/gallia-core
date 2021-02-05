package gallia.heads.grouping

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.actions.ActionsZZGrouping._

// ===========================================================================
trait HeadZGrouping { self: HeadZ =>
  import GroupingFluency._

  // ===========================================================================
  def grouping(conf: Start => End): Self = zz(GroupData.from(conf).thn(Grouping))

      private def hasAs(conf: Start => End): Self with HasAs = zzWithAs(GroupData.from(conf).thn(Grouping))

    // ===========================================================================
    def groupBy(groupers: RenWz)            : Self with HasAs = groupBy(_.explicit(groupers))
    def groupBy(grouper1: RenW, more: RenW*): Self with HasAs = groupBy(_.explicitFX(grouper1, more))
    def groupBy(groupers: GroupersSelection): Self with HasAs = hasAs(_.all.by(groupers).asDefault)

    // ===========================================================================
    def group   (groupee: RenW)             = groupOne(_.explicit(groupee))
    def groupOne(groupee: Groupee1Selection) = new _By1(groupee) // need a way to distinguish it from N...

    // ---------------------------------------------------------------------------
    def group(groupee1: RenW, groupee2: RenW, more: RenW*): _ByN = group(_.explicit(groupee1, groupee2, more:_*))
    def group(groupees: RenWz)                            : _ByN = group(_.explicit(groupees))
    def group(groupees: GroupeeNSelection)                : _ByN = groupN(resolveGroupees(groupees))

    private[heads] def groupN(groupees: TQRenz):_ByN = new _ByN(groupees)

      // ===========================================================================
      class _By1(groupee: Groupee1Selection) { // similar to conf's

        def by(groupers: RenWz)            : Self = by(_.explicit(groupers))
        def by(grouper1: RenW, more: RenW*): Self = by(_.explicitFX(grouper1, more))
        def by(groupers: GroupersSelection): Self = grouping(_.field(groupee).by(groupers))

        private[heads] def byN(groupers: TQRenz) : Self = grouping(_.field(groupee).byN(groupers))

        // ---------------------------------------------------------------------------
        def byTheRest: Self = self.grouping(_.field(groupee).byTheRest) }

      // ===========================================================================
      class _ByN(groupees: TQRenz) { // similar to conf's
        def by(groupers: RenWz)            : Self with HasAs = by(_.explicit(groupers))
        def by(grouper1: RenW, more: RenW*): Self with HasAs = by(_.explicitFX(grouper1, more))
        def by(groupers: GroupersSelection): Self with HasAs = hasAs(_.fields(groupees).by(groupers))

        // ---------------------------------------------------------------------------
        def byTheRest: Self with HasAs = hasAs(_.fields(groupees).byTheRest)
      }

}

// ===========================================================================

package gallia
package heads.grouping

import target._
import actions.ActionsZZAggregating._
import actions.ActionsZZStats0.Stats0
import heads.reducing.CountLikeType

// ===========================================================================
trait HeadZAggregations { self: HeadZ =>
    // TODO: t201019161520 - optimization: specialized version of sumby/countby/... rather than combos
    // TODO: t201208115422 - more, eg x.aggregateEach('f1, 'f2).wit(_.aggregates(_.sum, _.count)).by('g)

    // ===========================================================================
    @deprecated("see 210118083814 for new version") def stats(groupee: RenW) = new { //TODO: has as?
        private def _by(groupers: RenWz): HeadZ = zz(Stats0(groupee.value, groupers.renz, asOpt = None))
          def by(key : RenW)                         : HeadZ = _by(RenWz.from(key))
          def by(key1: RenW, key2: RenW, more: RenW*): HeadZ = _by(key1, key2, more)
          def by(keys: RenWz)                        : HeadZ = _by(keys) }

    // ===========================================================================
    // aggregate 1

    def aggregate(groupee: RenW)             : _Aggregate1 = aggregate(_.explicit(groupee))
    def aggregate(groupee: Groupee1Selection): _Aggregate1 = new _Aggregate1(resolveGroupee(groupee))

        // ---------------------------------------------------------------------------
        class _Aggregate1(groupee: TQRen) {
            def wit(f: ReducingType.type => ReducingType): __Aggregate1 = wit(f(ReducingType))
            def wit(rtipe: ReducingType)                 : __Aggregate1 = new __Aggregate1(groupee, rtipe) }

      // ---------------------------------------------------------------------------
      def count(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count)
      def count(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count)

      def sum  (groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.sum)
      def sum  (groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.sum)

      def mean (groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.mean)
      def mean (groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.mean)

      // ===========================================================================
      class __Aggregate1(groupee: TQRen, rtipe: ReducingType) {
          def by(key : GroupersSelection): HeadZ = zz(Agg1(groupee, resolveGroupers(key), rtipe))
          def by(keys: RenWz)            : HeadZ = by(_.explicit(keys))
          def by(key1: RenW, more: RenW*): HeadZ = by(_.explicitFX(key1, more)) }

    // ===========================================================================
    // aggregate N

    def aggregateEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggWith = aggregateEach(_.explicit(groupee1, groupee2, more:_*))
    def aggregateEach(groupees: RenWz)                            : AggWith = aggregateEach(_.explicit(groupees))
    def aggregateEach(groupees: GroupersSelection)                : AggWith = new AggWith(resolveGroupers(groupees))

      // ---------------------------------------------------------------------------
      def countEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggEachBy = countEach(_.explicit(groupee1, groupee2, more:_*))
      def countEach(groupees: RenWz)                            : AggEachBy = countEach(_.explicit(groupees))
      def countEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.count)

      // ---------------------------------------------------------------------------
      def sumEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggEachBy = sumEach(_.explicit(groupee1, groupee2, more:_*))
      def sumEach(groupees: RenWz)                            : AggEachBy = sumEach(_.explicit(groupees))
      def sumEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.sum)

      // ---------------------------------------------------------------------------
      def meanEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggEachBy = sumEach(_.explicit(groupee1, groupee2, more:_*))
      def meanEach(groupees: RenWz)                            : AggEachBy = sumEach(_.explicit(groupees))
      def meanEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.mean)

      // ===========================================================================
      class AggWith(groupees: TQRenz) {
          def wit(f: ReducingType.type => ReducingType): AggEachBy = wit(f(ReducingType))
          def wit(rtipe: ReducingType)                 : AggEachBy = new AggEachBy(groupees, rtipe) }

        // ===========================================================================
        class AggEachBy(groupees: TQRenz, rtipe: ReducingType) {

          def by(key : GroupersSelection): HeadZ with HasAs = zzWithAs(AggN(groupees, rtipe, resolveGroupers(key), asOpt = None))
          def by(keys: RenWz)            : HeadZ with HasAs = by(_.explicit(keys))
          def by(key1: RenW, more: RenW*): HeadZ with HasAs = by(_.explicitFX(key1, more)) }

    // ===========================================================================
    // all by

    def countWith(f: CountLikeType.type => CountLikeType): _CountBy = countWith(f(CountLikeType))
    def countWith(ctipe: CountLikeType)                  : _CountBy = new _CountBy(ctipe)

        // ---------------------------------------------------------------------------
        class _CountBy(ctipe: CountLikeType) {
          def by(groupers: GroupersSelection): Self with HasAs = zzWithAs(CountBy(resolveGroupers(groupers), ctipe, asOpt = None))
          def by(groupers: RenWz)            : Self with HasAs = by(_.explicit(groupers))
          def by(grouper1: RenW, more: RenW*): Self with HasAs = by(_.explicitFX(grouper1, more)) }

      // ---------------------------------------------------------------------------
      // TODO: t210131140932 - countBy needs to accept _.allKeys
      def   countBy(groupers: GroupersSelection): Self with HasAs = countWith(ReducingType.count).by(groupers)
        def countBy(groupers: RenWz)            : Self with HasAs = countBy(_.explicit(groupers))
        def countBy(grouper1: RenW, more: RenW*): Self with HasAs = countBy(_.explicitFX(grouper1, more))

        // ---------------------------------------------------------------------------
        // TODO: worth keeping these shorthands?
        def countDistinctBy(groupers: GroupersSelection): Self with HasAs = countWith(ReducingType.count_distinct).by(groupers)
        def countDistinctBy(groupers: RenWz)            : Self with HasAs = countDistinctBy(_.explicit(groupers))
        def countDistinctBy(grouper1: RenW, more: RenW*): Self with HasAs = countDistinctBy(_.explicitFX(grouper1, more))

        // ---------------------------------------------------------------------------
        def countPresentBy(groupers: GroupersSelection): Self with HasAs = countWith(ReducingType.count_present).by(groupers)
        def countPresentBy(groupers: RenWz)            : Self with HasAs = countPresentBy(_.explicit(groupers))
        def countPresentBy(grouper1: RenW, more: RenW*): Self with HasAs = countPresentBy(_.explicitFX(grouper1, more))

  // ===========================================================================
  // TODO:x
  private def _countBy(groupers: GroupersSelection, ctipe: CountLikeType): Self with HasAs =
        zzWithAs(CountBy(resolveGroupers(groupers), ctipe, asOpt = None))

      @deprecated private def _sumAllBy(groupers: GroupersSelection): Self with HasAs = ???//_countBy(groupers, ReducingType.sum)

      def sumAllBy(groupers: GroupersSelection)                : Self with HasAs = ???//_sumAllBy(groupers)
      def sumAllBy(groupers: RenWz)                            : Self with HasAs = ???//_sumAllBy(groupers)
      def sumAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = ???//_sumAllBy(grouper1, more)

}

// ===========================================================================

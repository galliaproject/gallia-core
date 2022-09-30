package gallia
package heads
package grouping

import target._
import actions.ActionsZZAggregating._
import heads.reducing.{ReducingPair, ReducingPair1, ReducingPairs, CountLikeType}

// ===========================================================================
trait HeadZAggregations { self: HeadZ =>
    // TODO: t201019161520 - optimization: specialized version of sumby/countby/... rather than combos
    // TODO: t201208115422 - more, eg x.aggregateEach('f1, 'f2).wit(_.aggregates(_.sum, _.count)).by('g)

    // ===========================================================================
    // aggregate 1

    def aggregate(groupee: RenW)             : _Aggregate1 = aggregate(_.explicit(groupee))
    def aggregate(groupee: Groupee1Selection): _Aggregate1 = new _Aggregate1(resolveGroupee(groupee))

    def aggregate(pairs: Seq[ReducingPair]): AggBy = new AggBy(ReducingPairs(pairs))
      def aggregate(pair1: ReducingPair1, more: ReducingPair1*): AggBy = aggregate(pair1 +: more)
      def aggregate(pair1: ReducingPair , more: ReducingPair *): AggBy = aggregate(pair1 +: more)
    
        // ---------------------------------------------------------------------------
        class _Aggregate1 private[HeadZAggregations] (groupee: TqRen) {
            def wit(f: ReducingType.type => ReducingType): __Aggregate1 = wit(f(ReducingType))
            def wit(rtipe: ReducingType)                 : __Aggregate1 = new __Aggregate1(groupee, rtipe) }

      // ---------------------------------------------------------------------------
      def stats(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.stats)
      def stats(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.stats)
      
      // ===========================================================================
      def count(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count_all)
      def count(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count_all)

        def countPresent(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count_present)
        def countPresent(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count_present)
        
        def countMissing(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count_missing)
        def countMissing(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count_missing)

        def countDistinct(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count_distinct)
        def countDistinct(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count_distinct)

        def countDistinctPresent(groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.count_distinct_present)
        def countDistinctPresent(groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.count_distinct_present)

      def sum  (groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.sum)
      def sum  (groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.sum)

      def mean (groupee: RenW)             : __Aggregate1 = aggregate(groupee).wit(_.mean)
      def mean (groupee: Groupee1Selection): __Aggregate1 = aggregate(groupee).wit(_.mean)

      // ===========================================================================
      class __Aggregate1 private[HeadZAggregations] (groupee: TqRen, rtipe: ReducingType) {
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
      def countEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.count_all)

      // ---------------------------------------------------------------------------
      def sumEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggEachBy = sumEach(_.explicit(groupee1, groupee2, more:_*))
      def sumEach(groupees: RenWz)                            : AggEachBy = sumEach(_.explicit(groupees))
      def sumEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.sum)

      // ---------------------------------------------------------------------------
      def meanEach(groupee1: RenW, groupee2: RenW, more: RenW*): AggEachBy = meanEach(_.explicit(groupee1, groupee2, more:_*))
      def meanEach(groupees: RenWz)                            : AggEachBy = meanEach(_.explicit(groupees))
      def meanEach(groupees: GroupersSelection)                : AggEachBy = new AggWith(resolveGroupers(groupees)).wit(ReducingType.mean)

      // ===========================================================================
      class AggWith private[HeadZAggregations] (groupees: TqRenz) { // TODO: homogenize with AggBy above
          def wit(f: ReducingType.type => ReducingType): AggEachBy = wit(f(ReducingType))
          def wit(rtipe: ReducingType)                 : AggEachBy = new AggEachBy(groupees, rtipe) }

        // ===========================================================================
        class AggBy private[HeadZAggregations] (pairs: ReducingPairs) { // TODO: homogenize with AggWith above
          def by(grouper: KeyW): HeadZ = { // TODO: add with HasAs
            self
              .retain(pairs.keyz.append(grouper.value))
              .groupBy(grouper.value).transformGroupEntitiesUsing { _.reduce(pairs)  } } }
        
        // ===========================================================================
        class AggEachBy private[HeadZAggregations] (groupees: TqRenz, rtipe: ReducingType) {
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
      def   countBy(groupers: GroupersSelection): Self with HasAs = countWith(ReducingType.count_all).by(groupers)
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

    // ---------------------------------------------------------------------------        
    private def _otherAllBy(rtipe: ReducingType)(groupers: RenWz): Self with HasAs = aggregateEach(_.allBut(groupers.fromz)).wit(rtipe).by(groupers)

      // ---------------------------------------------------------------------------
      def countAllBy(groupers: GroupersSelection)                : Self with HasAs = ??? // TODO
      def countAllBy(groupers: RenWz)                            : Self with HasAs = _otherAllBy(ReducingType.count_all)(groupers)
      def countAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = countAllBy(grouper1, grouper2, more:_*)

      // ---------------------------------------------------------------------------
      def sumAllBy(groupers: GroupersSelection)                : Self with HasAs = ??? // TODO
      def sumAllBy(groupers: RenWz)                            : Self with HasAs = _otherAllBy(ReducingType.sum)(groupers)
      def sumAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = sumAllBy(grouper1, grouper2, more:_*)

      // ---------------------------------------------------------------------------
      def meanAllBy(groupers: GroupersSelection)                : Self with HasAs = ??? // TODO
      def meanAllBy(groupers: RenWz)                            : Self with HasAs = _otherAllBy(ReducingType.mean)(groupers)
      def meanAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = meanAllBy(grouper1, grouper2, more:_*)

      // ---------------------------------------------------------------------------
      def stdevAllBy(groupers: GroupersSelection)                : Self with HasAs = ??? // TODO
      def stdevAllBy(groupers: RenWz)                            : Self with HasAs = _otherAllBy(ReducingType.stdev)(groupers)
      def stdevAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = stdevAllBy(grouper1, grouper2, more:_*)

      // ---------------------------------------------------------------------------
      def medianAllBy(groupers: GroupersSelection)                : Self with HasAs = ??? // TODO
      def medianAllBy(groupers: RenWz)                            : Self with HasAs = _otherAllBy(ReducingType.median)(groupers)
      def medianAllBy(grouper1: RenW, grouper2: RenW, more: RenW*): Self with HasAs = medianAllBy(grouper1, grouper2, more:_*)      
     
    // ===========================================================================
    def aggregateBy(key : GroupersSelection): _AggregateBy = new _AggregateBy(key, _agg)
    def aggregateBy(keys: RenWz)            : _AggregateBy = aggregateBy(_.explicit(keys))
    def aggregateBy(key1: RenW, more: RenW*): _AggregateBy = aggregateBy(_.explicitFX(key1, more))

      // ---------------------------------------------------------------------------
      class _AggregateBy private[HeadZAggregations] (groupers: GroupersSelection, as: Key) {
        def as(as: KeyW) = new _AggregateBy(groupers, as.value)

        // ---------------------------------------------------------------------------
        def using[T: WTT](f: HeadZ => HeadV[T]): HeadS =
          self
            .groupBy(groupers).as(as)
            .transformAllEntities(as).using(f)

        // ---------------------------------------------------------------------------
        def using(f: HeadZ => HeadU)(implicit d: DI): HeadS =
          self
            .groupBy(groupers).as(as)
            .transformAllEntities(as).using(f)
            .unnestAllFrom       (as)

        // ---------------------------------------------------------------------------
        import aptus.{Tuple2_, Tuple3_, Tuple4_, Tuple5_}
        private type E = GenericEntry[HeadV[_]]

          def using(f: HeadZ =>  E)             (implicit d1: DI, d2: DI)                                : HeadS = using { x => Seq(f(x)).pipe(gallia.headO) }(d1)
          def using(f: HeadZ => (E, E))         (implicit d1: DI, d2: DI, d3: DI)                        : HeadS = using { f(_).toSeq.pipe(gallia.headO) }(d1)
          def using(f: HeadZ => (E, E, E))      (implicit d1: DI, d2: DI, d3: DI, d4: DI)                : HeadS = using { f(_).toSeq.pipe(gallia.headO) }(d1)
          def using(f: HeadZ => (E, E, E, E))   (implicit d1: DI, d2: DI, d3: DI, d4: DI, d5: DI)        : HeadS = using { f(_).toSeq.pipe(gallia.headO) }(d1)
          def using(f: HeadZ => (E, E, E, E, E))(implicit d1: DI, d2: DI, d3: DI, d4: DI, d5: DI, d6: DI): HeadS = using { f(_).toSeq.pipe(gallia.headO) }(d1)
          // for >5: use gallia.mergeAll explicitly

        // ---------------------------------------------------------------------------
        def using(pair1: ReducingPair, more: ReducingPair*): HeadS =
          self
            .groupBy(groupers)
            .transformGroupEntitiesUsing { 
              _.reduce(pair1, more:_*) }
            .unnestAllFromGroup }

}

// ===========================================================================

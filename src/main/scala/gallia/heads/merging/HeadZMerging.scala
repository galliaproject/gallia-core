package gallia.heads.merging

import gallia._
import gallia.actions.ActionsZZMerging._

// ===========================================================================
trait HeadZMerging { self: HeadZ =>
  import MergingData.JoinKey
  import MergingFluency._

  // ---------------------------------------------------------------------------
  // TODO: t210117143536 - move to .merging(that)(_.union)?
  @Distributivity // TODO: prepend/append if not distributed
  def union(that: HeadU): HeadZ = that.convertToZ.pipe(union)
  def union(that: HeadZ): HeadZ = zzToz(that)(UnionZ)

  // ===========================================================================
  def merging(that: HeadZ)(conf: Start => End): HeadZ = zzToz(that)(MergingData.from(conf).pipe(Merging))

    // ---------------------------------------------------------------------------
    //TODO:
    // - t201124153838: a bring "guaranteed" (no missing); or by default?
    // - t201124153839: likewise a join guaranteed? when not inner?
    def bring(that: HeadZ)(conf: Bring => End): HeadZ = merging(that)(_.bring.pipe(conf))

      def bring   (that: HeadZ, target: KeyW, via: KeyW)   : HeadZ = bring(that)(_.fields(target).via(via))
      def bring   (that: HeadZ, target: KeyW, via: JoinKey): HeadZ = bring(that)(_.fields(target).via(via))
      def bring   (that: HeadZ, target: KeyW)              : HeadZ = bring(that)(_.fields(target).viaCommonKey)

      //TODO: or as bringAll().via(???)
      def bringAll(that: HeadZ,                via: KeyW)   : HeadZ = bring(that)(_.all.via(via))
      def bringAll(that: HeadZ,                via: JoinKey): HeadZ = bring(that)(_.all.via(via))
      def bringAll(that: HeadZ)                             : HeadZ = bring(that)(_.all.viaCommonKey)

    // ===========================================================================
    // - t210128130124 - p2 - allow explicit use of partially in-memory join if one side is small enough ("hash" join)

    def coGroup(that: HeadZ)(conf: CoGroup => End): HeadZ = merging(that)(_.coGroup.pipe(conf))

      def  fullCoGroup(that: HeadZ)(conf: CoGroupOn => End): HeadZ = coGroup(that)(_.full .pipe(conf))
      def  leftCoGroup(that: HeadZ)(conf: CoGroupOn => End): HeadZ = coGroup(that)(_.left .pipe(conf))
      def rightCoGroup(that: HeadZ)(conf: CoGroupOn => End): HeadZ = coGroup(that)(_.right.pipe(conf))
      def innerCoGroup(that: HeadZ)(conf: CoGroupOn => End): HeadZ = coGroup(that)(_.inner.pipe(conf))

        def  fullCoGroup(that: HeadZ, on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ =  fullCoGroup(that)(_.on(on).as(asLeft, asRight))
        def  leftCoGroup(that: HeadZ, on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ =  leftCoGroup(that)(_.on(on).as(asLeft, asRight))
        def rightCoGroup(that: HeadZ, on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ = rightCoGroup(that)(_.on(on).as(asLeft, asRight))
        def innerCoGroup(that: HeadZ, on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ = innerCoGroup(that)(_.on(on).as(asLeft, asRight))

      // ===========================================================================
      def join(that: HeadZ)(conf: Join => End) = merging(that)(_.join.pipe(conf))

        def  fullJoin(that: HeadZ): HeadZ =  join(that)(_. full.onCommonKey)
        def  leftJoin(that: HeadZ): HeadZ =  join(that)(_. left.onCommonKey)
        def rightJoin(that: HeadZ): HeadZ =  join(that)(_.right.onCommonKey)
        def innerJoin(that: HeadZ): HeadZ =  join(that)(_.inner.onCommonKey)

          def  fullJoin(that: HeadZ, on: KeyW): HeadZ = join(that)(_. full.on(on))
          def  leftJoin(that: HeadZ, on: KeyW): HeadZ = join(that)(_. left.on(on))
          def rightJoin(that: HeadZ, on: KeyW): HeadZ = join(that)(_.right.on(on))
          def innerJoin(that: HeadZ, on: KeyW): HeadZ = join(that)(_.inner.on(on))

          def  fullJoin(that: HeadZ, on: JoinKey): HeadZ = join(that)(_. full.on(on))
          def  leftJoin(that: HeadZ, on: JoinKey): HeadZ = join(that)(_. left.on(on))
          def rightJoin(that: HeadZ, on: JoinKey): HeadZ = join(that)(_.right.on(on))
          def innerJoin(that: HeadZ, on: JoinKey): HeadZ = join(that)(_.inner.on(on))

  //def innerJoin(that: HeadZ, onLeft: KeyW, onRight: KeyW): HeadZ = innerJoin(that, JoinKey(onLeft, onright)) // TODO: worth it shorthand?
  //...

  // ===========================================================================
  // worth keeping right?
  def  fullJoin(pair1: (HeadZ, JoinKey), more: (HeadZ, JoinKey)*): HeadZ =  fullJoin(pair1 +: more)
  def  leftJoin(pair1: (HeadZ, JoinKey), more: (HeadZ, JoinKey)*): HeadZ =  leftJoin(pair1 +: more)
  def rightJoin(pair1: (HeadZ, JoinKey), more: (HeadZ, JoinKey)*): HeadZ = rightJoin(pair1 +: more)
  def innerJoin(pair1: (HeadZ, JoinKey), more: (HeadZ, JoinKey)*): HeadZ = innerJoin(pair1 +: more)

  def  fullJoin(pairs: Seq[(HeadZ, JoinKey)])                    : HeadZ = pairs.foldLeft(self) { case (curr, (that, joinKey)) => curr. fullJoin(that, joinKey) }
  def  leftJoin(pairs: Seq[(HeadZ, JoinKey)])                    : HeadZ = pairs.foldLeft(self) { case (curr, (that, joinKey)) => curr. leftJoin(that, joinKey) }
  def rightJoin(pairs: Seq[(HeadZ, JoinKey)])                    : HeadZ = pairs.foldLeft(self) { case (curr, (that, joinKey)) => curr.rightJoin(that, joinKey) }
  def innerJoin(pairs: Seq[(HeadZ, JoinKey)])                    : HeadZ = pairs.foldLeft(self) { case (curr, (that, joinKey)) => curr.innerJoin(that, joinKey) }
}

// ===========================================================================
object HeadZPair { // TODO: t201123125604 - why implicit not picked up?
    //implicit def to0(x: HeadZ, y: HeadZ): HeadZPair = ???
    //implicit def to1(x: (HeadZ, HeadZ)): HeadZPair = new HeadZPair(x._1, x._2)
    //implicit def to2(x: (BObjs, BObjs)): HeadZPair = new HeadZPair(x._1, x._2)
  }

  // ===========================================================================
  class HeadZPair(left: HeadZ, right: HeadZ) { import MergingFluency._

    // ---------------------------------------------------------------------------
    def merging(conf: Start => End): HeadZ = left.merging(right)(conf)

      def union: HeadZ = left.union(right)

      // ---------------------------------------------------------------------------
      def coGroup(conf: CoGroup => End): HeadZ = merging(_.coGroup.pipe(conf))

        def  fullCoGroup(conf: CoGroupOn => End): HeadZ = coGroup(_.full .pipe(conf))
        def  leftCoGroup(conf: CoGroupOn => End): HeadZ = coGroup(_.left .pipe(conf))
        def rightCoGroup(conf: CoGroupOn => End): HeadZ = coGroup(_.right.pipe(conf))
        def innerCoGroup(conf: CoGroupOn => End): HeadZ = coGroup(_.inner.pipe(conf))

          def  fullCoGroup(on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ =  fullCoGroup(_.on(on).as(asLeft, asRight))
          def  leftCoGroup(on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ =  leftCoGroup(_.on(on).as(asLeft, asRight))
          def rightCoGroup(on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ = rightCoGroup(_.on(on).as(asLeft, asRight))
          def innerCoGroup(on: KeyW, asLeft: KeyW = _left, asRight: KeyW = _right): HeadZ = innerCoGroup(_.on(on).as(asLeft, asRight))
         
        // ---------------------------------------------------------------------------
        def join(conf: Join => End) = left.join(right)(conf)

          def  fullJoin(conf: JoinOn => End): HeadZ = left.join(right)(_. full.pipe(conf))
          def  leftJoin(conf: JoinOn => End): HeadZ = left.join(right)(_. left.pipe(conf))
          def rightJoin(conf: JoinOn => End): HeadZ = left.join(right)(_.right.pipe(conf))
          def innerJoin(conf: JoinOn => End): HeadZ = left.join(right)(_.inner.pipe(conf))

            def  fullJoin(on: KeyW): HeadZ = left.join(right)(_. full.on(on))
            def  leftJoin(on: KeyW): HeadZ = left.join(right)(_. left.on(on))
            def rightJoin(on: KeyW): HeadZ = left.join(right)(_.right.on(on))
            def innerJoin(on: KeyW): HeadZ = left.join(right)(_.inner.on(on))

              def  fullJoinOnCommonKey: HeadZ = left.join(right)(_. full.onCommonKey)            
              def  leftJoinOnCommonKey: HeadZ = left.join(right)(_. left.onCommonKey)
              def rightJoinOnCommonKey: HeadZ = left.join(right)(_.right.onCommonKey)
              def innerJoinOnCommonKey: HeadZ = left.join(right)(_.inner.onCommonKey)

    // ---------------------------------------------------------------------------
    // TODO: more such shorthands
    def innerJoinOn(commonKey: Key)             : HeadZ = innerJoin(_.on(commonKey))
    def innerJoinOn(leftKey: Key, rightKey: Key): HeadZ = innerJoin(_.on(leftKey, rightKey))
  }

// ===========================================================================

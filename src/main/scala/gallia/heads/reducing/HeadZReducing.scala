package gallia
package heads.reducing

import actions.ActionsReducing._

// ===========================================================================
trait HeadZReducing { ignored: HeadZ =>
  type Selection      = SEL.Reducing.Selector
  private val resolve = SEL.Reducing.resolve _

  // ---------------------------------------------------------------------------
  // note: this effectively retain

  def reduce(key1: KeyW, more: KeyW*): _Reduce = reduce(_.explicitFX(key1, more))
  def reduce(keys: KeyWz)            : _Reduce = reduce(_.explicit(keys))
  def reduce(sel: Selection)         : _Reduce = new _Reduce(resolve(sel))

  // ---------------------------------------------------------------------------
    class _Reduce(targets: TqKeyz) {
      //TODO: t201209144743 - allow .wit(_.aggregates(_.mean, _.sum) ... or as .wit(_mean, _sum, ...)
      def wit(f: ReducingType.type => ReducingType): HeadU = wit(f(ReducingType))
      def wit(rtipe: ReducingType)                 : HeadU = zu(Reduce1(targets, rtipe)) }

    // ---------------------------------------------------------------------------
    //TODO: vs toArray: def reduceWithGrouping(key1: KeyW, key2: KeyW, more: KeyW*): HeadU = reduce(key1, key2, more:_*).wit(_.grouping)

    def reduceWithCount(key1: KeyW, more: KeyW*): HeadU = reduce(key1, more:_*).wit(_.count)
    def reduceWithCount(keys: KeyWz)            : HeadU = reduce(keys         ).wit(_.count)
    def reduceWithCount(sel: Selection)         : HeadU = reduce(sel          ).wit(_.count)

    def reduceWithSum  (key1: KeyW, more: KeyW*): HeadU = reduce(key1, more:_*).wit(_.sum)
    def reduceWithSum  (keys: KeyWz)            : HeadU = reduce(keys         ).wit(_.sum)
    def reduceWithSum  (sel: Selection)         : HeadU = reduce(sel          ).wit(_.sum)

    def reduceWithMean (key1: KeyW, more: KeyW*): HeadU = reduce(key1, more:_*).wit(_.mean)
    def reduceWithMean (keys: KeyWz)            : HeadU = reduce(keys         ).wit(_.mean)
    def reduceWithMean (sel: Selection)         : HeadU = reduce(sel          ).wit(_.mean)

    def reduceWithStats(key1: KeyW, more: KeyW*): HeadU = reduce(key1, more:_*).wit(_.stats)
    def reduceWithStats(keys: KeyWz)            : HeadU = reduce(keys         ).wit(_.stats)
    def reduceWithStats(sel: Selection)         : HeadU = reduce(sel          ).wit(_.stats)

    // ---------------------------------------------------------------------------
    def stats = reduceWithStats(_.allKeys)

  // ===========================================================================
  def reduce(pairs:     ReducingPairs): HeadU = zu(Reduce(pairs))

    def reduce(pairs: Seq[ReducingPair])                  : HeadU = reduce(ReducingPairs(pairs))
    def reduce(pair1: ReducingPair1, more: ReducingPair1*): HeadU = reduce(pair1 +: more)
    def reduce(pair1: ReducingPair , more: ReducingPair *): HeadU = reduce(pair1 +: more)

  // TODO: custom reduce
}

// ===========================================================================

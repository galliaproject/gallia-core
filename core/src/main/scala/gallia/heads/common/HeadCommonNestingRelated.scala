package gallia
package heads
package common

import aptus.Separator

import actions.common.ActionsCommonNestingRelated._

// ===========================================================================
trait HeadCommonNestingRelated[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  // TODO: see t210109144926 - generalize nest/unnest as "move"

  // ---------------------------------------------------------------------------
  // nesting

  def nest(x: RPathW)                             = new _Nest(RPathWz.from(x))
  def nest(x1: RPathW, x2: RPathW, more: RPathW*) = new _Nest(x1, x2, more)
  def nest(xs: RPathWz)                           = new _Nest(xs)
  def nest(sel: SEL.Nest.Selector)                = new _Nest(SEL.Nest.resolve(sel))

    // ---------------------------------------------------------------------------
    final class _Nest(targets: TqRPathz) {
      def under(     newNestingKey: KeyW): Self2 = self2 :+ new NestUnder(targets, newNestingKey     .value)
      def into (existingNestingKey: RenW): Self2 = self2 :+ new NestInto (targets, existingNestingKey.value) }

  // ===========================================================================
  // unnesting; note: renaming is N/A

  // ---------------------------------------------------------------------------
  def unnestAllFrom(parent: KPathW): Self2 = unnestFrom(parent).fields(_.allKeys)
  def unnestAllFromGroup           : Self2 = unnestAllFrom(_group) // common after custom aggregations

    // ===========================================================================
    def unnestFrom(parent: KPathW) = new _UnnestFrom(parent)

      class _UnnestFrom private[HeadCommonNestingRelated] (parent: KPathW) {
        /** uses nested name(s) */
        def field(key: RenW): Self2 = fields(_.explicit(key))

        // ---------------------------------------------------------------------------
        def fields(key1: RenW, key2: RenW, more: RenW*): Self2 = fields(_.explicit(key1, key2, more:_*))
        def fields(keys: RenWz)                        : Self2 = fields(_.explicit(keys))

        // ---------------------------------------------------------------------------
        def fields(sel: SEL.UnnestFrom.Selector): Self2 = self2 :+ UnnestFrom(parent.value, SEL.UnnestFrom.resolve(sel)) }

    // ===========================================================================
    /** uses nesting name; OOO = Objects Of Ones, eg {"a": [{"b": 1}, {"b": 2}]} */
    def unnestOOO(path: KPathW): F = // TODO: t210109144447 - too convoluted, create dedicated action
        path.value.initPair match {
          case (None      , leaf) => self2                                 .unnestOOOLeaf(leaf)
          case (Some(tail), leaf) => self2.transformEntityx(tail).using { _.unnestOOOLeaf(leaf) } }

      // ---------------------------------------------------------------------------
      private[heads] def unnestOOOLeaf(key: Key): F =
        self2
          .transformAllEntities(key)
            .using(_.renameSoleKey(to = key)) // TODO: validate only one key
          .rename(key).to(_tmp)
          .unnestAllFrom(_tmp)

  // ===========================================================================
  // renesting; TODO: t210109173114 - renest just up to n levels?

  def renest(f: KeyW)                        : _Renest = renest(_.explicit(f))
  def renest(fs: KeyWz)                      : _Renest = renest(_.explicit(fs))
  def renest(f1: KeyW, f2: KeyW, more: KeyW*): _Renest = renest(_.explicit(f1, f2, more:_*))
  def renest(sel: SEL.Renesting.Selector)    : _Renest = new _Renest(SEL.Renesting.resolve(sel))

  // ---------------------------------------------------------------------------
  def renestAllKeys                      : _Renest = renest(_.allKeys)
  def renestIfKeys(pred: SKey => Boolean): _Renest = renest(_.filterKeys(pred))

    // ===========================================================================
    class _Renest(targets: TqKeyz) { // TODO: just as "using"?
      //TODO: t210109175621 - reproduce the optional "as" mechanism
      def usingDefaultSeparator         : Self2 = usingSeparator("_")
      def usingSeparator(sep: Separator): Self2 = self2 :+ Renest(targets, sep) }

  // ===========================================================================
  // TODO: see t210109144926 - generalize nest/unnest as "move"
  @deprecated("WIP") def move(x: RPathW)                             = new _Move(RPathWz.from(x))
  @deprecated("WIP") def move(x1: RPathW, x2: RPathW, more: RPathW*) = new _Move(x1, x2, more)
  @deprecated("WIP") def move(xs: RPathWz)                           = new _Move(xs)

    // ---------------------------------------------------------------------------
    final class _Move(targets: RPathWz) {
      def to(destination: RPathW): Self2 = self2 :+ new Move(targets.rpathz, Some(destination.value))
      def toCurrent              : Self2 = self2 :+ new Move(targets.rpathz, None) }
}

// ===========================================================================

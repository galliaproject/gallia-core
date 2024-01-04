package gallia
package selection
package untyped
package processors

import aptus.{Anything_, String_, Seq_}
import aptus.Option_

import vldt._Error

// ===========================================================================
object UtsProcessors {
  private val _utils =  UtsProcessorsUtils

  // ===========================================================================
  case object SoleKey extends KeySelection {
    def vldt(c: Cls): Errs = if (c.size != 1) _Error.MoreThanOneKey(c.keys).errs else Nil //err(s"${gallia.vldt.ErrorId.MoreThanOneKey}: ${c.keys.@@}") else Nil

    // ---------------------------------------------------------------------------
             def key (c: Cls): Key         = c.keys.head
    override def key_(c: Cls): Option[Key] = if (vldt(c).nonEmpty) None else c.keys.force.one.in.some }

  // ===========================================================================
  case class ExplicitKey(value: Key) extends KeySelection {
      def vldt(c: Cls)     : Errs = Nil//TODO
      def key(ignored: Cls): Key = value }

    // ---------------------------------------------------------------------------
    case class ExplicitRen(value: Ren) extends RenSelection {
      def vldt(c: Cls): Errs = Nil//TODO:if (!c.keySet.contains(value)) err("TODO") else Nil
      def ren(ignored: Cls): Ren = value }

    // ---------------------------------------------------------------------------
    case class ExplicitKPath(value: KPath) extends KPathSelection {
      def vldt(c: Cls): Errs = Nil//TODO:if (!c.keySet.contains(value)) err("TODO") else Nil
      def kpath(ignored: Cls): KPath = value }

    // ---------------------------------------------------------------------------
    case class ExplicitRPath(value: RPath) extends RPathSelection {
      def vldt(c: Cls): Errs = Nil//TODO:if (!c.keySet.contains(value)) err("TODO") else Nil
      def rpath(ignored: Cls): RPath = value }

  // ===========================================================================
  case class ExplicitIndex(value: MIndex, special: Option[ExplicitIndex.Special]) extends KeySelection {
      private def resolve(c: Cls) = _utils.unmirror(c.size)(value)

      def vldt(c: Cls): Errs = resolve(c).pipe { resolved => if (c.size <= resolved) _Error.OutOfBoundKey(c.size, Seq(resolved)).errs else Nil }
      def key (c: Cls): Key  = resolve(c).pipe(c.keys) }

    // ---------------------------------------------------------------------------
    object ExplicitIndex {
      type Special = Special.Value
      object Special extends Enumeration {
        val
            First,
            Second,
            Third,
            SecondToLast,
            Last
          = Value
      }
    }

  // ===========================================================================
  //TODO: check duplicates?
  case class ExplicitKeyz(values: Keyz) extends KeyzSelection {
      def vldt(c: Cls): Errs = Nil//TODO?
      def keys(c: Cls): Keys = values.values }

    case class ExplicitRenz(values: Renz) extends RenzSelection {
      def vldt(c: Cls): Errs = Nil
      def renz(c: Cls): Renz = values }

    case class ExplicitKPathz(values: KPathz) extends KPathzSelection {
      def vldt(c: Cls): Errs = Nil
      def kpaths(c: Cls): KPaths = values.values }

    case class ExplicitRPathz(values: RPathz) extends RPathzSelection {
      def vldt  (c: Cls): Errs = Nil
      def rpathz(c: Cls): RPathz = values }

  // ===========================================================================
  sealed trait ExplicitIndicesBase extends KeyzSelection {
      val values: Seq[MIndex]

      // ---------------------------------------------------------------------------
      def vldt(c: Cls): Errs = // note: duplicates are handled in actions
        _utils
          .outOfBounds(c.size)(values)
          .in.noneIf(_.isEmpty).toSeq
          .map(_Error.OutOfBoundKey(c.size, _)).map(_.err)

      // ---------------------------------------------------------------------------
      protected def validUnmirroredIndices(c: Cls): Seq[Index] =
        values
          .diff(_utils.outOfBounds(c.size)(values))
          .map (_utils.unmirror   (c.size))
    }

    // ---------------------------------------------------------------------------
    case class ExplicitIndices(values: Seq[MIndex]) extends ExplicitIndicesBase {
      def keys(c: Cls): Keys = validUnmirroredIndices(c).pipe(_.map(c.keyVector.apply)) }

    case class AllButIndices(values: Seq[MIndex]) extends ExplicitIndicesBase {
      def keys(c: Cls): Keys = validUnmirroredIndices(c).pipe { x => Range(0, c.size).diff(x).map(c.keyVector.apply) } }

  // ===========================================================================
  case class AllButKeys(values: Keyz) extends KeyzSelection { //TODO: specials: allbut first/last
      def vldt(c: Cls): Errs = Nil //TODO: check duplicates + check some left?
      def keys(c: Cls): Keys = c.keys.diff(values.values) }

    // ---------------------------------------------------------------------------
    case class AllButKeys2(groupee: trgt.TqRen) extends KeyzSelection {
      def vldt(c: Cls): Errs = Nil //TODO: check duplicates + check some left?
      def keys(c: Cls): Keys = groupee.resolve(c).from.pipe { x => c.keys.filterNot(_ == x) } }

  // ===========================================================================
  case object AllKeys   extends Errorless with KeyzSelection   { def keys  (c: Cls): Keys   = c.keys }
  case object LeafPaths extends Errorless with KPathzSelection { def kpaths(c: Cls): KPaths = c.leafPaths }
  case object AllPaths  extends Errorless with KPathzSelection { def kpaths(c: Cls): KPaths = c.allPaths }

  // ===========================================================================
  //TODO: allow no matches?
  case class KeyPredicate1(pred: SKey => Boolean) extends Errorless with KeySelection {
      def key(c: Cls): Key = c.skeys.find(pred).map(_.symbol).force }

    case class KeyPredicateN(pred: SKey => Boolean) extends Errorless with KeyzSelection {
      def keys(c: Cls): Keys = c.skeys.filter(pred).map(_.symbol) }

    case class PathPredicate1(pred: KPath => Boolean) extends Errorless with KPathSelection {
      def kpath(c: Cls): KPath = c.allPaths.find(pred).force }

    case class PathPredicateN(pred: KPath => Boolean) extends Errorless with KPathzSelection {
      def kpaths(c: Cls): Seq[KPath] = c.allPaths.filter(pred) }

  // ===========================================================================
  class IfType[T: WTT] extends KeyzSelection { // TODO: special: int, ...
      def vldt(c: Cls): Errs = Nil//TODO; disasllow BObj (only basic ones); 201014103336
      def keys(c: Cls): Keys = _utils.ifType[T](c) }

    // ---------------------------------------------------------------------------
    class IfTypeRecursively[T: WTT] extends KPathzSelection {
      def vldt  (c: Cls): Errs = Nil//TODO; disasllow BObj (only basic ones); 201014103336
      def kpaths(c: Cls): KPaths = _utils.ifTypeRecursively[T](OptionalKPath.Root)(c) }

  // ===========================================================================
  class CustomSimpleKeyFunction (f: Seq[SKey] => SKey) extends Errorless with KeySelection {
        def key(c: Cls): Key = f(c.skeys).symbol }

    // ---------------------------------------------------------------------------
    class CustomAdvancedToKeyFunction(f: Cls => Key) extends Errorless with KeySelection {
        def key(c: Cls): Key = f(c) }

    // ---------------------------------------------------------------------------
    class CustomSimpleKeysFunction (f: Seq[SKey] => Seq[SKey]) extends Errorless with KeyzSelection {
        def keys(c: Cls): Keys = f(c.skeys).map(_.symbol) }

    // ---------------------------------------------------------------------------
    class CustomAdvancedToKeysFunction(f: Cls => Keys) extends Errorless with KeyzSelection {
        def keys(c: Cls): Keys = f(c) }

    // ---------------------------------------------------------------------------
    case class CustomSimpleLeafPathsFunction (f: KPaths => KPaths) extends KPathzSelection {
        def vldt(c: Cls): Errs = Nil//TODO
        def kpaths(c: Cls): KPaths = f(c.leafPaths) }

      // ---------------------------------------------------------------------------
      case class CustomSimpleAllPathsFunction (f: KPaths => KPaths) extends KPathzSelection {
        def vldt(c: Cls): Errs = Nil//TODO: check subset, no duplicates, ...
        def kpaths(c: Cls): KPaths = f(c.allPaths) }

    // ---------------------------------------------------------------------------
    class CustomAdvancedToPathsFunction(f: Cls => KPaths) extends KPathzSelection {
      def vldt(c: Cls): Errs = Nil//TODO
      def kpaths(c: Cls): KPaths = f(c) }

}

// ===========================================================================

package gallia.actions

import aptus.Anything_
import aptus.Separator

import gallia._
import gallia.target._
import gallia.atoms.AtomsUUSomewhatBasics._

// ===========================================================================
object ActionsUUSomewhatBasics {
  import gallia.actions.utils.{ActionsUUHelper => _helper}
  import gallia.actions.utils.ActionsUtils._

  // ===========================================================================
  case class RemoveConditionally(target: TtqRPathz, pred: Any => Boolean) extends ActionUUb {
      def  vldt(c: Cls): Errs = // TODO: no need to check if some fields are left
        if (target.node.isNone) _Error.CantBeNone.errs //TODO: also Nil
        else                    target.vldtAsOrigin(c, mode = SpecialCardiMode.IgnoreRequiredness)

      def _meta  (c: Cls): Cls     = target.tq.qpathz_(c).foldLeft(c)(_ toNonRequired _)
      def atomuus(c: Cls): AtomUUs = target.tq.qpathz_(c).thn(_atoms(_RemoveIf(_, pred ))) }

    // ===========================================================================
    case class RemoveConditionallyWhatever(target: TqRPathz, value: Any) extends ActionUUb {
      def  vldt(c: Cls): Errs = target.vldtAsOrigin(c) // TODO: no need to check if some fields are left

      def _meta  (c: Cls): Cls     = target.qpathz_(c).foldLeft(c)(_ toNonRequired _)
      def atomuus(c: Cls): AtomUUs = target.qpathz_(c).thn(_atoms(_RemoveWhateverIf(_, value))) }

  // ===========================================================================
  case class SetDefault(target: TtqRPathz, value: AnyValue) extends ActionUUb {
    def  vldt(c: Cls): Errs =
      target.vldtAsOrigin(c) ++
      _vldt.typeCompatibility(c, target.duo(c), SpecialCardiMode.Normal)
      //TODO: check was opt? what about multiple?

    def _meta(c: Cls): Cls   = target.qpathz_(c).foldLeft(c)(_ toRequired _)
    def atomuus(c: Cls): AtomUUs = target.qpathz_(c).thn(_atoms(_SetDefault(_, value)))
  }

  // ===========================================================================
  case class Split(paths: RPathz, splitter: StringSplitter) extends ActionUUa with TodoV1 {
    def _meta(c: Cls): Cls  = paths.foldLeft(c)(_ toMultiple _)
    def atomuus: AtomUUs = _atoms(paths, _Split(_, splitter))
  }

  // ===========================================================================
  /** strict = all values are translated, therefore type can change */
  case class Translate(target: TtqRPathz, to: TypeNode, strict: Boolean, mapping: Seq[(_, _)]) extends ActionUUb {
      @deprecated val toOpt = to.in.someIf(_ != target.node)

      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c, SpecialCardiMode.IgnoreRequiredness /* TODO: check no Some/None provided */) ++
        (toOpt match {
          case None     => Nil
          case Some(to) =>
            if (strict) {
              _vldt.validType(gallia.vldt.Location.Root, to)
            } else {
              if (to != target.node) Some(Err("201105140603 - MustBeSameType")) else None
              //_vldt.typeCompatibilities6(c, target, ignoreRequiredness = false)
            }
          })

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls =
        target.qpathz_(c).thn { qpathz =>
          toOpt match {
            case None     => c.rename    (qpathz)
            case Some(to) => c.updateType(qpathz, to) } }

      // ---------------------------------------------------------------------------
      def atomuus(c: Cls): AtomUUs = target.qpathz_(c).thn(_atoms(c)(_TransformVV(_, _helper.Translate.wrap(mapping, toOpt))))
    }

  // ===========================================================================
  case class SingleSwap(parentOpt: Option[RPath], target1: Key, target2: Key) extends ActionUUa with TodoV1 {
      def _meta(c: Cls): Cls  = c.swapFields(parentOpt, target1, target2)
      def atomuus =
        parentOpt match {
          case None         =>                                    Seq(                   _Swap(target1, target2) )
          case Some(parent) => potentialRenaming(parent).toSeq ++ Seq(_Nested(parent.to, _Swap(target1, target2))) }
    }

    // ---------------------------------------------------------------------------
    class MultiSwap(targets: Seq[KeyWPair]) extends ActionUUa with TodoV1 {
      def _meta(c: Cls): Cls  = targets.map(_.value).foldLeft(c)((curr, x) => curr.swapFields(None, x._1, x._2))
      def atomuus = targets.map(_.value).map(_Swap.tupled)
    }

  // ---------------------------------------------------------------------------
  class CopyEntries(origin: RPath, destinations: Seq[Key]) extends ActionUUa with TodoV1 {
      def _meta(c: Cls): Cls  = destinations.foldLeft(c)(_.copyField(origin.from, _)).rename(origin)
      def atomuus =
        origin.initPair2 match {
          case (None        , Left(key))     => destinations.map(_Copy(key, _))
          case (None        , Right(actual)) => destinations.map(_Copy(actual.from, _)) :+ _Rename(actual)
          case (Some(parent), Left(key))     => destinations.map { destination => _Nested(parent, _Copy(key        , destination)) }
          case (Some(parent), Right(actual)) => destinations.map { destination => _Nested(parent, _Copy(actual.from, destination)) } :+ _Nested(parent, _Rename(actual)) }
    }

  // ===========================================================================
  case class ZipStrings(keys: Renz, sep: Separator, newNestingKey : Key) extends ActionUUd with TodoV1 { //TODO: validate at least 2 keys
      def _meta(c: Cls): Cls = c.zipStrings(keys, newNestingKey)
      def atomuu = _ZipStrings(keys, sep, newNestingKey) }

    // ---------------------------------------------------------------------------
    case class _ZipStrings(keys: Renz, sep: Separator, newNestingKey : Key) extends AtomUU { def naive(o: Obj) =
      gallia.atoms.AtomsUUZip.zip(o, keys, sep, newNestingKey) }

  // ===========================================================================
  case class EnsureNumeric(targets: TqKPathz) extends IdentityM1 with IdentityUUa { // TODO: generalize to any validation
    def vldt(c: Cls): Errs  = targets.resolve(c).toSeq.flatMap(_vldt.checkNotNumerical(c, _)) }

}

// ===========================================================================

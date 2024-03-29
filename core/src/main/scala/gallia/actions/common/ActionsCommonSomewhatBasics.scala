package gallia
package actions
package common

import aptus.Anything_
import aptus.Separator

import trgt._
import atoms.common.AtomsCommonSomewhatBasics._

// ===========================================================================
object ActionsCommonSomewhatBasics {
  import gallia.actions.utils.{ActionsCommonHelper => _helper}
  import gallia.actions.utils.ActionsUtils._

  // ===========================================================================
  case class RemoveConditionally(target: TtqRPathz, pred: Any => Boolean) extends ActionUU1N {
      def  vldt(c: Cls): Errs = // TODO: no need to check if some fields are left
        if (target.typeNode.isNone) _Error.CantBeNone.errs //TODO: also Nil
        else                        target.vldtAsOrigin(c, mode = SpecialCardiMode.IgnoreRequiredness)

      def _meta  (c: Cls): Cls     = target.tq.rpathz_(c).foldLeft(c)(_ toOptional _)
      def atomuus(c: Cls): AtomUUs = target.tq.rpathz_(c).pipe(_atoms(x => _RemoveIf(x, x, pred ))) }

    // ===========================================================================
    case class RemoveConditionallyWhatever(target: TqRPathz, value: Any) extends ActionUU1N {
      def  vldt(c: Cls): Errs = target.vldtAsOrigin(c) // TODO: no need to check if some fields are left

      def _meta  (c: Cls): Cls     = target.rpathz_(c).foldLeft(c)(_ toOptional _)
      def atomuus(c: Cls): AtomUUs = target.rpathz_(c).pipe(_atoms(_RemoveWhateverIf(_, value))) }

    // ===========================================================================
    case class RemoveConditionally2(reference: TypedTargetQuery[Key], target: TqKey, pred: Any => Boolean) extends ActionUU11 {
      def  vldt (c: Cls): Errs   = reference.vldtAsOrigin(c) ++ target.vldtAsOrigin(c)
      def _meta (c: Cls): Cls    = c.toOptional(target.resolve(c))
      def atomuu(c: Cls): AtomUU = _RemoveIf(reference.resolve(c), target.resolve(c), pred) }

    // ---------------------------------------------------------------------------
    case class RemoveConditionally2Whatever(reference: Key, target: TqKey, value: Any) extends ActionUU11 {
      def  vldt (c: Cls): Errs   = _vldt.fieldPresence(c, reference).toSeq ++ target.vldtAsOrigin(c)
      def _meta (c: Cls): Cls    = c.toOptional(target.resolve(c))
      def atomuu(c: Cls): AtomUU = _RemoveIf(reference, target.resolve(c), _ == value) }

  // ===========================================================================
  case class SetDefaultValueFor(target: TtqRPathz, newValue: AnyValue) extends ActionUU1N {
        def vldt(c: Cls): Errs =
          target.vldtAsOrigin(c) ++
          _vldt.typeCompatibility(c, target.duo(c), SpecialCardiMode.Normal)
        //TODO: check was opt? what about multiple?

        def _meta  (c: Cls): Cls     = target.rpathz_(c).foldLeft(c)(_ toRequired _)
        def atomuus(c: Cls): AtomUUs = target.rpathz_(c).pipe(_atoms(_SetDefault(_, newValue))) }

      // ---------------------------------------------------------------------------
      case class SetDefaultConditionally2(
            reference: TypedTargetQuery[Key], target: TqKey, pred: Any => Boolean, newValueType: TypeNode, newValue: Any)
          extends IdentityM1 with ActionUU11 {
        def vldt(c: Cls): Errs =
          reference.vldtAsOrigin(c) ++
          target.vldtAsOrigin(c).orIfEmpty {
            _vldt.typeCompatibility(c, target.duo(c, newValueType), SpecialCardiMode.IgnoreRequiredness) }

        // ---------------------------------------------------------------------------
        def atomuu(c: Cls): AtomUU = _SetDefault2(reference.resolve(c), target.resolve(c), pred, newValue) }

      // ---------------------------------------------------------------------------
      case class SetDefaultConditionally2Whatever(
            reference: Key, target: TqKey, referenceValue: Any, newValueType: TypeNode, newValue: Any)
          extends IdentityM1 with ActionUU11 {

        def vldt(c: Cls): Errs   =
          _vldt.fieldPresence(c, reference).toSeq ++
          target.vldtAsOrigin(c).orIfEmpty {
            _vldt.typeCompatibility(c, target.duo(c, newValueType), SpecialCardiMode.IgnoreRequiredness) }

        // ---------------------------------------------------------------------------
        def atomuu(c: Cls): AtomUU = _SetDefault2(reference, target.resolve(c), _ == referenceValue, newValue) }

  // ===========================================================================
  case class Split(paths: RPathz, splitter: StringSplitter) extends ActionUU0N with TodoV1 {
    def _meta(c: Cls): Cls  = paths.foldLeft(c)(_ toMultiple _)
    def atomuus: AtomUUs = _atoms(paths, _Split(_, splitter)) }

  // ===========================================================================
  /** strict = all values are translated, therefore type can change */
  case class Translate(target: TtqRPathz, to: TypeNode, strict: Boolean, mapping: Seq[(_, _)]) extends ActionUU1N {
      @deprecated val toOpt = to.in.someIf(_ != target.typeNode)

      def  vldt(c: Cls): Errs =
        target.vldtAsOrigin(c, SpecialCardiMode.IgnoreRequiredness /* TODO: check no Some/None provided */) ++
        (toOpt match {
          case None     => Nil
          case Some(to) =>
            if (strict) {
              _vldt.validType(gallia.vldt.Location.Root, to)
            } else {
              if (to != target.typeNode) Some(Err("201105140603 - MustBeSameType")) else None
              //_vldt.typeCompatibilities6(c, target, ignoreRequiredness = false)
            }
          })

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls =
        target.rpathz_(c).pipe { rpathz =>
          toOpt match {
            case None     => c.rename    (rpathz)
            case Some(to) => c.updateInfo(rpathz, to.forceNonBObjInfo) } }

      // ---------------------------------------------------------------------------
      def atomuus(c: Cls): AtomUUs = target.rpathz_(c).pipe(_atoms(c)(_TransformVV(_, _helper.Translate.wrap(mapping, toOpt))))
    }

  // ===========================================================================
  case class SingleSwap(parentOpt: Option[RPath], target1: Key, target2: Key) extends ActionUU0N with TodoV1 {
      def _meta(c: Cls): Cls  = c.swapFields(parentOpt, target1, target2)
      def atomuus =
        parentOpt match {
          case None         =>                                    Seq(                   _Swap(target1, target2) )
          case Some(parent) => potentialRenaming(parent).toSeq ++ Seq(_Nested(parent.to, _Swap(target1, target2))) } }

  // ---------------------------------------------------------------------------
  class CopyEntries(origin: RPath, destinations: Seq[Key]) extends ActionUU0N with TodoV1 {
      def _meta(c: Cls): Cls  = destinations.foldLeft(c)(_.copyField(origin.from, _)).rename(origin)
      def atomuus =
        origin.initPair2 match {
          case (None        , Left(key))     => destinations.map(_Copy(key, _))
          case (None        , Right(actual)) => destinations.map(_Copy(actual.from, _)) :+ _Rename(actual)
          case (Some(parent), Left(key))     => destinations.map { destination => _Nested(parent, _Copy(key        , destination)) }
          case (Some(parent), Right(actual)) => destinations.map { destination => _Nested(parent, _Copy(actual.from, destination)) } :+ _Nested(parent, _Rename(actual)) } }

  // ===========================================================================
  case class ZipStrings(keys: Renz, sep: Separator, newNestingKey : Key) extends ActionUU01 with TodoV1 { //TODO: validate at least 2 keys
      def _meta(c: Cls): Cls = c.zipStrings(keys, newNestingKey)
      def atomuu = _ZipStrings(keys, sep, newNestingKey) }

    // ---------------------------------------------------------------------------
    case class _ZipStrings(keys: Renz, sep: Separator, newNestingKey : Key) extends AtomUU { def naive(o: Obj) =
      gallia.atoms.common.AtomsCommonZip.zip(o, keys, sep, newNestingKey) }

  // ===========================================================================
  case class EnsureNumeric(targets: TqKPathz) extends IdentityM1 with IdentityUU0N { // TODO: generalize to any validation
    def vldt(c: Cls): Errs  = targets.resolve(c).toSeq.flatMap(_vldt.checkNotNumerical(c, _)) }

}

// ===========================================================================

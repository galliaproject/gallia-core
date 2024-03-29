package gallia
package actions

import aptus.Anything_

import trgt._
import plans.Clss
import FunctionWrappers._
import data.multiple.Streamer
import atoms.AtomsOthers._
import atoms.AtomsUV._
import atoms.AtomsZZ._
import atoms.AtomsCustom._CustomZZ

// ===========================================================================
object ActionsOthers {

  object Output extends ActionVM1 with ActionAN {
    def  vldt(in: Cls) = Nil
    def _meta(in: Cls) = in
    def atoms(ignored: ActionMetaContext): Atoms = Nil }

  // ===========================================================================
  case class ModifyUnderlyingStreamer(f: Streamer[Obj] => Streamer[Obj]) extends IdentityVM1 with ActionZZ01 {
      def atomzz = _CustomZZ(_._modifyUnderlyingStreamer(f)) }

    // ---------------------------------------------------------------------------
    case object ToViewBased     extends IdentityVM1 with ActionZZ01 { def atomzz = _ToViewBased     }
    case object ToIteratorBased extends IdentityVM1 with ActionZZ01 { def atomzz = _ToIteratorBased }

  // ===========================================================================
  //TODO: t210115175106 - versions that allows configuring more
  case class InspectU(msg: Option[String], abort: Boolean) extends IdentityVM1 with ActionUU01 {
      def atomuu = _InspectU(msg, abort) }

    case class InspectZ(msg: Option[String], abort: Boolean) extends IdentityVM1 with ActionZZ01 {
      def atomzz = _InspectZ(msg, abort) }

  // ===========================================================================  
  case class Unpivot(keyz: Keyz) extends ActionUU01 with TodoV1 /* origins + check compatible infos */ {
      def _meta(c: Cls): Cls    = c.unpivot(keyz)
      def atomuu       : AtomUU =  _Unpivot(keyz) }

    // ---------------------------------------------------------------------------
    //TODO: t220914144753 - generalize as unpivot of some keys
    case class UnpivotOneItem(key1: Key, key2: Key, targetStringValue: String) extends ActionUU11 {
      def  vldt(c: Cls): Errs = Nil // TODO: t240124123030 - missing validations

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = {
        val f = c.field(key1)

        f .subInfo1
          .forceNestedClass
          .remove(key2)
          .pipe { nc => c.add(
            if (f.isOptional) targetStringValue.cls_(nc)
            else              targetStringValue.cls (nc)) } }

      // ---------------------------------------------------------------------------
      def atomuu(c: Cls): AtomUU = _UnpivotOneItem(key1, key2, targetStringValue) }

  // ===========================================================================
  /** a purely meta operation */
  case class ModifyEnumValuesFor(target: TqRPathz, f: Seq[EnumValue] => Seq[EnumValue]) extends IdentityUU0N {
    def  vldt(c: Cls): Errs = // allows no change
      target.vldtAsOrigin(c)
        .orIfEmpty { _vldt.checkIsEnumField       (c)(target) }
        .orIfEmpty { _vldt.checkAreValidEnumValues(c)(target)(f) }

    // ---------------------------------------------------------------------------
    def _meta(c: Cls): Cls  = target.rpathz_(c).foldLeft(c) { _.transformSoleValueType(_) {
      case BasicType._Enm(values) => BasicType._Enm(f(values)) } } }

  // ===========================================================================
  case object UnionUU extends ActionV2 with ActionM2 with ActionAN {
    def  vldt (in1: Cls, in2: Cls): Errs  = Nil // TODO: eg collisions
    def _meta (in1: Cls, in2: Cls): Cls   = in1.mergeDisjoint(in2)
    def atoms(ignored: ActionMetaContext): Atoms = _Merge.in.seq }

  // ===========================================================================
  // uz

  case object ConvertUtoZ extends ActionUZ01 with IdentityVM1 {
    def atomuz: AtomUZ = _ConvertUtoZ }

  // ---------------------------------------------------------------------------
  abstract class FlattenBy(target: KPath) {
      def vldt (in: Cls): Errs = Nil//TODO; check was seq?; ensure only one level of multiplicity
      def _meta(in: Cls): Cls = in.toSingle(target).pipeIf(in.isOptional(target))(_.toOptional(target)) }

    // ---------------------------------------------------------------------------
    case class FlattenByU(target: KPath) extends FlattenBy(target) with ActionUZ {
      def atomuzs(ignored: ActionMetaContext): AtomUZs =
        _FlattenByU(target).in.seq }

    case class FlattenByZ(target: KPath) extends FlattenBy(target) with ActionZZ {
      def atomzzs(ignored: ActionMetaContext): AtomZZs =
        _FlattenByZ(target).in.seq }

  // ===========================================================================
  // zu

  case object MergeAll extends ActionZU with TodoV1 {
    def _meta(in: Cls): Cls = ???//in.reduceLeft(_ mergeDisjoint _) - FIXME:?
    def atomzus(ctx: ActionMetaContext): AtomZUs = ??? } //_MergeAll.in.seq

  // ---------------------------------------------------------------------------
  case object AsArray1 extends ActionZU with TodoV1 { // TODO: key sole + not array
      def _meta(in: Cls): Cls =  in.soleField.key.pipe(in.toMultiple(_))
      def atomzus(ctx: ActionMetaContext): AtomZUs = ctx.afferents.forceOne.soleField.key.pipe(_AsArray1).in.seq }

    // ---------------------------------------------------------------------------
    case class AsArray2(newKey: Key) extends ActionZU with TodoV1 {
      def _meta(in: Cls): Cls =  in.nest(in.keyz.renz, newKey).toMultiple(newKey)
      def atomzus(ctx: ActionMetaContext): AtomZUs = _AsArray2(newKey).in.seq }

  // ---------------------------------------------------------------------------
  case object ForceOne extends ActionZU {
    def vldt (in: Cls): Errs = Nil
    def _meta(in: Cls): Cls = in
    def atomzus(ignored: ActionMetaContext): AtomZUs = _ForceOne.in.seq }

  // ===========================================================================
  //FIXME t210115175242 - runtime validation of newKeys for these

  case class Pivone(
          newKeys  : Keyz, // only for meta
          keyKey   : Key , // only for data - may or may not have the same cardinality as newKeys - TODO: t210303102200 - also allow selection
          valueKey : TqKPath /* TODO: key only? */)
        extends ActionZU11 {
      private def resolveTargetKey(c: Cls): Key = valueKey.resolve(c).forceLeafFX /* see  t210303111953 */

      // ---------------------------------------------------------------------------
      def vldt  (in: Cls): Errs   = Nil //TODO: t210303101704 - check reasonnably "to-textable" value
      def _meta (in: Cls): Cls    = resolveTargetKey(in).pipe { in.unarrayEntries(newKeys, _) }

      // uniqueness already ensured, see 240124153043
      def atomzu(in: Cls): AtomZU = resolveTargetKey(in).pipe { target => _Pivone(keyKey, target) } }

  // ===========================================================================
  // vv

  case class MapV2V(to: TypeNode, f: _ff11) extends ActionVV {
    def  vldt(in: Clss): Errs = Nil // TODO: t240124123030 - missing validations
    def _meta(in: Clss): Cls  = in.forceOne//TODO: ok? t220627162134 - must adapt type
    def atoms (ignored: ActionMetaContext): Atoms = _MapV2V(f).in.seq }

  // ---------------------------------------------------------------------------
  case class CombineVV(x: TypeNode, y: TypeNode, result: TypeNode, f: _ff21) extends ActionVvToV {
    //FIXME: validate valid result type (result)
    def _meta(x: Cls, y: Cls) = x // FIXME: t220718112037
    def dataz2(c1: Cls , c2: Cls) = _CombineVV(f).in.seq }

  // ===========================================================================
  // vu
  case class DressUp[T: WTT](key: Key) extends TodoV1 with ActionVU01 {
    def _meta(ignored: Cls): Cls = Fld.fld[T](key).toSoleFieldCls
    def atomvu = new AtomVU { def naive(v: Vle): Obj = obj(key -> v) } }

  // ===========================================================================
  // uv

  case class GrabU(from: TtqKPath1, checkOrigin: Boolean /* not for V=Any */) 
    extends Grab[AtomUV](from, checkOrigin, _GrabUOpt, _GrabUOne) with ActionUV
    
  // ---------------------------------------------------------------------------
  case class PopulateDataClass(node: TypeNode) extends ActionUV {
    def  vldt(in     : Clss): Errs = Nil // TODO: t240124123030 - missing validations
    def _meta(ignored: Clss): Cls  = ???
    def atoms(ctx: ActionMetaContext): Atoms = ??? } //_PopulateDataClass

  // ---------------------------------------------------------------------------
  case class SquashUUnsafe(to: TypeNode, f: Obj => Any) extends ActionUV {
      def  vldt(in     : Clss): Errs = Nil // TODO: t240124123030 - missing validations
      def _meta(ignored: Clss): Cls  = Cls.vle(to)
      def atoms(ignored: ActionMetaContext): Atoms = _SquashUUnsafe(f).in.seq }

    // ---------------------------------------------------------------------------
    case class SquashU1(from: TtqKPath1, to: TypeNode, f: _ff11) extends ActionUV with SquashXN { // == Grab1
      def atom(c: Cls) = _SquashU1(from.pathPairT(c), from.wrapc(to, f)) }

    case class SquashU2(from: TtqKPath2, to: TypeNode, f: _ff21) extends ActionUV with SquashXN {
      def atom(c: Cls) = _SquashU2(from.pathPairT(c), from.wrapc(to, f)) }

    case class SquashU3(from: TtqKPath3, to: TypeNode, f: _ff31) extends ActionUV with SquashXN {
      def atom(c: Cls) = _SquashU3(from.pathPairT(c), from.wrapc(to, f)) }

    case class SquashU4(from: TtqKPath4, to: TypeNode, f: _ff41) extends ActionUV with SquashXN {
      def atom(c: Cls) = _SquashU4(from.pathPairT(c), from.wrapc(to, f)) }

    case class SquashU5(from: TtqKPath5, to: TypeNode, f: _ff51) extends ActionUV with SquashXN {
      def atom(c: Cls) = _SquashU5(from.pathPairT(c), from.wrapc(to, f)) }

  // ===========================================================================
  // zv

  case object Size extends ActionZV {
    def  vldt(ignored: Clss): Errs = Nil // TODO: t240124123030 - missing validations
    def _meta(ignored: Clss): Cls  = Cls.vleInt
    def atoms(ctx: ActionMetaContext): Atoms = _Size.in.seq }

  // ---------------------------------------------------------------------------
  case class GrabZ(from: TtqKPath1 /* does not specify the surrounding Seq */, checkOrigin: Boolean /* not for V=Any */) 
    extends Grab[AtomZV](from, checkOrigin, _GrabZOpt, _GrabZOne) with ActionZV

  // ---------------------------------------------------------------------------
  case class SquashZUnsafe(to: TypeNode, f: Seq[Obj] => Any) extends ActionZV {
      def  vldt(in     : Clss): Errs = Nil // TODO: t240124123030 - missing validations
      def _meta(ignored: Clss): Cls  = Cls.vle(to)
      def atoms(ignored: ActionMetaContext): Atoms = _SquashZUnsafe(f).in.seq }

    // ---------------------------------------------------------------------------
    case class SquashZ1(from: TtqKPath, to: TypeNode, f: _agg1) extends ActionZV with SquashXN {
      def atom(c: Cls) = _SquashZ1(from.pathPairT(c), f) }

    case class SquashZ2(from: TtqKPath2, to: TypeNode, f: _agg2) extends ActionZV with SquashXN {
      def atom(c: Cls) = _SquashZ2(from.pathPairT(c), f) }

    case class SquashZ3(from: TtqKPath3, to: TypeNode, f: _agg3) extends ActionZV with SquashXN {
      def atom(c: Cls) = _SquashZ3(from.pathPairT(c), f) }

    case class SquashZ4(from: TtqKPath4, to: TypeNode, f: _agg4) extends ActionZV with SquashXN {
      def atom(c: Cls) = _SquashZ4(from.pathPairT(c), f) }

    case class SquashZ5(from: TtqKPath5, to: TypeNode, f: _agg5) extends ActionZV with SquashXN {
      def atom(c: Cls) = _SquashZ5(from.pathPairT(c), f) }

  // ===========================================================================
  case class CheckpointU(path1: String, path2: String) extends ActionUU11 with IdentityVM1 {
      def atomuu(c: Cls) = _CheckpointU(c, path1, path2) }
    
    case class CheckpointZ(path1: String, path2: String) extends ActionZZ11 with IdentityVM1 {
      def atomzz(c: Cls) = _CheckpointZ(c, path1, path2) }

  // ===========================================================================
  abstract class Grab[$Atom <: Atom](
      from       : TtqKPath1,
      checkOrigin: Boolean /* not for V=Any */,
      ifOpt      : KPath => $Atom,
      ifOne      : KPath => $Atom) {
    def  vldt(in: Clss): Errs = if (!checkOrigin) Nil else from.vldtAsOrigin(in.forceOne) //TODO: more
    def _meta(in: Clss): Cls  = if (!checkOrigin) in.forceOne else Cls.vles(from.typeNode)

    // ---------------------------------------------------------------------------
    def atoms(ctx: ActionMetaContext): Atoms =
      ctx.afferents.forceOne
        .pipe(from.pathPairT)
        .pipe { pair =>
          if (pair.optional) Seq(ifOpt(pair.path))
          else               Seq(ifOne(pair.path)) } } }

// ===========================================================================

package gallia
package actions

import aptus.{Anything_, Seq_}
import aptus.Separator
import target._
import FunctionWrappers._
import data.multiple.Streamer
import atoms._UWrapper
import atoms.AtomsOthers._
import atoms.AtomsUV._
import atoms.AtomsZZ._EnsureUniquenessBy
import atoms.AtomsCustom._CustomZZ

// ===========================================================================
object ActionsOthers {

  object Output extends ActionVM1 with ActionAN {
    def  vldt(in: Cls) = Nil
    def _meta(in: Cls) = in
    def atoms(ignored: NodeMetaContext): Atoms = Nil }

  // ===========================================================================
  case class UWrapper(u: ActionUU) extends ActionZZ {
    def vldt (in: Cls ): Errs = u. vldt(in)
    def _meta(in: Cls ): Cls  = u._meta(in)
    def atomzzs(ctx: NodeMetaContext) = u.atomuus(ctx).map(_UWrapper)
  }

  // ===========================================================================
  case class ModifyUnderlyingStreamer(f: Streamer[Obj] => Streamer[Obj]) extends IdentityVM1 with ActionZZd {
      def atomzz = _CustomZZ(_._modifyUnderlyingStreamer(f)) }

    // ---------------------------------------------------------------------------
    case object AsListBased extends IdentityVM1 with ActionZZd {
      def atomzz = _CustomZZ(_._asListBased) }

  // ===========================================================================
  //TODO: t210115175106 - versions that allows configuring more
  case class InspectU(msg: Option[String], abort: Boolean) extends IdentityVM1 with ActionUUd {
      def atomuu = _InspectU(msg, abort) }

    case class InspectZ(msg: Option[String], abort: Boolean) extends IdentityVM1 with ActionZZd {
      def atomzz = _InspectZ(msg, abort) }

  // ===========================================================================  
  case class Unpivot(keyz: Keyz) extends ActionUUd with TodoV1 /* origins + check compatible infos */ {
    def _meta(c: Cls): Cls    = c.unpivot(keyz)        
    def atomuu       : AtomUU =  _Unpivot(keyz) }

  // ===========================================================================
  /** a purely meta operation */
  case class ModifyEnumValuesFor(target: TqRPathz, f: Seq[EnumValue] => Seq[EnumValue]) extends IdentityUUa {
    def  vldt(c: Cls): Errs = // allows no change
      target.vldtAsOrigin(c)
        .orIfEmpty { _vldt.checkIsEnumField       (c)(target) }
        .orIfEmpty { _vldt.checkAreValidEnumValues(c)(target)(f) }

    // ---------------------------------------------------------------------------
    def _meta(c: Cls): Cls  = target.qpathz_(c).foldLeft(c) { _.transformSoleValueType(_) {
      case BasicType._Enm(values) => BasicType._Enm(f(values)) } } }

  // ===========================================================================
  case object UnionUU extends Action with ActionV2 with ActionM2 with ActionAN {
    def  vldt (in1: Cls, in2: Cls): Errs  = Nil // TODO: eg collisions
    def _meta (in1: Cls, in2: Cls): Cls   = in1.mergeDisjoint(in2)
    def atoms(ignored: NodeMetaContext): Atoms = _Merge.in.seq }

  // ===========================================================================
  // uz

  case object ConvertUtoZ extends ActionUZd with IdentityVM1 {
    def atomuz: AtomUZ = _ConvertUtoZ }

  // ---------------------------------------------------------------------------
  abstract class FlattenBy(target: KPath) extends ActionUZ {
      def vldt (in: Cls): Errs = Nil//TODO; check was seq?; ensure only one level of multiplicity
      def _meta(in: Cls): Cls = in.toNonMultiple(target).pipeIf(in.isOptional(target))(_.toNonRequired(target)) }

    // ---------------------------------------------------------------------------
    case class FlattenByU(target: KPath) extends FlattenBy(target) with ActionUZ {
      def atoms(ignored: NodeMetaContext): Atoms =
        _FlattenByU(target).in.seq }

    case class FlattenByZ(target: KPath) extends FlattenBy(target) with ActionZZ {
      def atomzzs(ignored: NodeMetaContext): AtomZZs = 
        _FlattenByZ(target).in.seq }

  // ===========================================================================
  // zu

  case object MergeAll extends ActionZU with TodoV1 {
    def _meta(in: Cls): Cls = ???//in.reduceLeft(_ mergeDisjoint _) - FIXME:?
    def atoms(ctx: NodeMetaContext): Atoms = ??? } //_MergeAll.in.seq

  // ---------------------------------------------------------------------------
  case object AsArray1 extends ActionZU with TodoV1 { // TODO: key sole + not array
      def _meta(in: Cls): Cls =  in.soleField.key.pipe(in.toMultiple(_))
      def atoms(ctx: NodeMetaContext): Atoms = ctx.forceSingleAfferent.soleField.key.pipe(_AsArray1).in.seq }

    // ---------------------------------------------------------------------------
    case class AsArray2(newKey: Key) extends ActionZU with TodoV1 {
      def _meta(in: Cls): Cls =  in.nest(in.keyz.renz, newKey).toMultiple(newKey)
      def atoms(ctx: NodeMetaContext): Atoms = _AsArray2(newKey).in.seq }

  // ---------------------------------------------------------------------------
  case object ForceOne extends ActionZU {
    def vldt (in: Cls): Errs = Nil
    def _meta(in: Cls): Cls = in
    def atoms(ignored: NodeMetaContext): Atoms = _ForceOne.in.seq }

  // ===========================================================================
  //FIXME t210115175242 - runtime validation of newKeys for these
  case class Pivone(
          newKeys  : Keyz, // only for meta
          keyKey   : Key , // only for data - may or may not have the same cardinality as newKeys - TODO: t210303102200 - also allow selection
          valueKey : TqKPath /* TODO: key only? */)
        extends ActionZU {
      private def resolveTargetKey(c: Cls): Key = valueKey.resolve(c).forceLeafFX /* see  t210303111953 */
      
      // ---------------------------------------------------------------------------
      def vldt (in: Cls)             : Errs  = Nil //TODO: t210303101704 - check reasonnably "to-textable" value
      def _meta(in: Cls)             : Cls   = resolveTargetKey(in)                     .pipe(in.unarrayEntries(newKeys, _))
      def atoms(ctx: NodeMetaContext): Atoms = resolveTargetKey(ctx.forceSingleAfferent).pipe { key =>
        Seq(
          _EnsureUniquenessBy(Keyz.from(keyKey)),
          _Pivone(keyKey, key)) } }

    // ---------------------------------------------------------------------------
    @deprecated case class UnarrayEntries0(
            newKeys  : Keyz, // only for meta
            keyKeys  : Keyz, // only for data - may or may not have the same cardinality as newKeys
            separator: Separator, // for data only, and only needed if more than one "key" key
            valueKey : Key)
          extends ActionZU {
        def vldt (in: Cls): Errs = Nil //TODO; consistency of key separator; reasonnable separator; check "textable" value; check separator availble if more than 1 keykey
        def _meta(in: Cls): Cls = valueKey.pipe(in.unarrayEntries(newKeys, _))
  
        def atoms(ignored: NodeMetaContext): Atoms = Seq(
            _EnsureUniquenessBy(keyKeys),
            _UnarrayEntries0(keyKeys, separator, valueKey) ) }
  
    // ---------------------------------------------------------------------------
    @deprecated("see 210303104417") case class UnarrayBy0(
          newKeys: Keyz, // only for meta
          keys   : Keyz,
          sep    : Separator) // for data only
        extends ActionZU {
      def vldt (in: Cls): Errs = Nil //TODO
      def _meta(in: Cls): Cls = in.unarrayBy0(keys, newKeys)
      def atoms(ignored: NodeMetaContext): Atoms = Seq(
        _EnsureUniquenessBy(keys),
        _UnarrayBy0(keys, sep)) }

  // ===========================================================================
  // vv

  case class MapV2V(to: TypeNode, f: _ff11) extends ActionVV {
    def vldt(in: Seq[Cls] ) = Nil // TODO
    def _meta(in: Seq[Cls] ) = in.force.one//TODO: ok?
    def atoms (ignored: NodeMetaContext): Atoms = _MapV2V(f).in.seq }

  // ===========================================================================
  // uv

  case class GrabU(from: TtqKPath1, checkOrigin: Boolean /* not for V=Any */) 
    extends Grab[AtomUV](from, checkOrigin, _GrabUOpt, _GrabUOne) with ActionUV
    
  // ---------------------------------------------------------------------------
  case class PopulateDataClass(node: TypeNode) extends ActionUV {
    def vldt (in: Seq[Cls]): Errs = ???
    def _meta(ignored: Seq[Cls]): Cls = ???
    def atoms(ctx: NodeMetaContext): Atoms = ??? } //_PopulateDataClass

  // ---------------------------------------------------------------------------
  case class SquashUUnsafe(to: TypeNode, f: Obj => Any) extends ActionUV {
      def vldt (in: Seq[Cls]): Errs = Nil//TODO
      def _meta(ignored: Seq[Cls]): Cls = Cls.vle(to)
      def atoms(ignored: NodeMetaContext): Atoms = _SquashUUnsafe(f).in.seq }

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
    def vldt (ignored: Seq[Cls]): Errs = Nil// TODO
    def _meta(ignored: Seq[Cls]): Cls  = Cls.vleInt
    def atoms(ctx: NodeMetaContext): Atoms = _Size.in.seq }

  // ---------------------------------------------------------------------------
  case class GrabZ(from: TtqKPath1 /* does not specify the surrounding Seq */, checkOrigin: Boolean /* not for V=Any */) 
    extends Grab[AtomZV](from, checkOrigin, _GrabZOpt, _GrabZOne) with ActionZV // {

  // ---------------------------------------------------------------------------
  case class SquashZUnsafe(to: TypeNode, f: Seq[Obj] => Any) extends ActionZV {
      def vldt (in: Seq[Cls]): Errs = Nil//TODO
      def _meta(ignored: Seq[Cls]): Cls = Cls.vle(to)
      def atoms(ignored: NodeMetaContext): Atoms = _SquashZUnsafe(f).in.seq }

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
  case class CheckpointU(path1: String, path2: String) extends ActionUUc with IdentityVM1 {
      def atomuu(c: Cls) = _CheckpointU(c, path1, path2) }
    
    case class CheckpointZ(path1: String, path2: String) extends ActionZZc with IdentityVM1 {
      def atomzz(c: Cls) = _CheckpointZ(c, path1, path2) }

  // ===========================================================================
  abstract class Grab[$Atom <: Atom](
      from       : TtqKPath1,
      checkOrigin: Boolean /* not for V=Any */,
      ifOpt      : KPath => $Atom,
      ifOne      : KPath => $Atom) {
    def vldt (in: Seq[Cls]): Errs = if (!checkOrigin) Nil     else from.vldtAsOrigin(in.force.one) //TODO: more
    def _meta(in: Seq[Cls]): Cls  = if (!checkOrigin) in.head else Cls.vles(from.node)

    // ---------------------------------------------------------------------------
    def atoms(ctx: NodeMetaContext): Atoms =
      ctx.afferents.force.one
        .pipe(from.pathPairT)
        .pipe { pair =>
          if (pair.optional) Seq(ifOpt(pair.path))
          else               Seq(ifOne(pair.path))  } }
    
}

// ===========================================================================

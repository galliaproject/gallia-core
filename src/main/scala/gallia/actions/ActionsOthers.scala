package gallia.actions

import aptus.{Anything_, Seq_}
import aptus.Separator

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.data.multiple.Streamer
import gallia.atoms._UWrapper
import gallia.atoms.AtomsOthers._
import gallia.atoms.AtomsUV._
import gallia.atoms.AtomsZZ._EnsureUniquenessBy
import gallia.atoms.AtomsCustom._CustomZZ

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
  case object UnionUU extends Action with ActionV2 with ActionM2 with ActionAN {
    def  vldt (in1: Cls, in2: Cls): Errs  = Nil // TODO: eg collisions
    def _meta (in1: Cls, in2: Cls): Cls   = in1.mergeDisjoint(in2)
    def atoms(ignored: NodeMetaContext): Atoms = _Merge.as.seq }

  // ===========================================================================
  // uz

  case object ConvertUtoZ extends ActionUZd with IdentityVM1 {
    def atomuz: AtomUZ = _ConvertUtoZ }

  // ---------------------------------------------------------------------------
  abstract class FlattenBy(target: RPath) extends ActionUZ {
      def vldt (in: Cls): Errs = Nil//TODO; check was seq?; ensure only one level of multiplicity
      def _meta(in: Cls): Cls = in.rename(target).toNonMultiple(target.to).thnIf(in.isOptional(target.from))(_.toNonRequired(target.to)) }

    // ---------------------------------------------------------------------------
    case class FlattenByU(target: RPath) extends FlattenBy(target) with ActionUZ {
      def atoms(ignored: NodeMetaContext): Atoms = _FlattenByU(target.fromFX).as.seq } //TODO: + rename

    case class FlattenByZ(target: RPath) extends FlattenBy(target) with ActionZZ {
      def atomzzs(ignored: NodeMetaContext): AtomZZs = _FlattenByZ(target.fromFX).as.seq } //TODO: + rename

  // ===========================================================================
  // zu

  case object MergeAll extends ActionZU with TodoV1 {
    def _meta(in: Cls): Cls = ???//in.reduceLeft(_ mergeDisjoint _) - FIXME:?
    def atoms(ctx: NodeMetaContext): Atoms = ??? } //_MergeAll.as.seq

  // ---------------------------------------------------------------------------
  case object AsArray1 extends ActionZU with TodoV1 { // TODO: key sole + not array
      def _meta(in: Cls): Cls =  in.soleField.key.thn(in.toMultiple(_))
      def atoms(ctx: NodeMetaContext): Atoms = ctx.forceSingleAfferent.soleField.key.thn(_AsArray1).as.seq }

    // ---------------------------------------------------------------------------
    case class AsArray2(newKey: Key) extends ActionZU with TodoV1 {
      def _meta(in: Cls): Cls =  in.nest(in.keyz.renz, newKey).toMultiple(newKey)
      def atoms(ctx: NodeMetaContext): Atoms = _AsArray2(newKey).as.seq }

  // ---------------------------------------------------------------------------
  case object ForceOne extends ActionZU {
    def vldt (in: Cls): Errs = Nil
    def _meta(in: Cls): Cls = in
    def atoms(ignored: NodeMetaContext): Atoms = _ForceOne.as.seq }

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
      def _meta(in: Cls)             : Cls   = resolveTargetKey(in)                     .thn(in.unarrayEntries(newKeys, _))
      def atoms(ctx: NodeMetaContext): Atoms = resolveTargetKey(ctx.forceSingleAfferent).thn { key =>
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
        def _meta(in: Cls): Cls = valueKey.thn(in.unarrayEntries(newKeys, _))
  
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
    def atoms (ignored: gallia.NodeMetaContext): Atoms = _MapV2V(f).as.seq }

  // ===========================================================================
  // uv

  case class GrabU(from: TtqKPath1) extends ActionUV {
    def vldt (in: Seq[Cls]): Errs = from.vldtAsOrigin(in.force.one) //TODO: more
    def _meta(ignored: Seq[Cls]): Cls = Cls.vle(from.node)
    def atoms(ctx: NodeMetaContext): Atoms = { val c = ctx.afferents.force.one; _GrabU(from.pathPairT(c)).as.seq } }

  // ---------------------------------------------------------------------------
  case class PopulateDataClass(node: TypeNode) extends ActionUV {
    def vldt (in: Seq[Cls]): Errs = ???
    def _meta(ignored: Seq[Cls]): Cls = ???
    def atoms(ctx: NodeMetaContext): Atoms = ??? } //_PopulateDataClass

  // ---------------------------------------------------------------------------
  case class SquashUUnsafe(to: TypeNode, f: Obj => Any) extends ActionUV {
      def vldt (in: Seq[Cls]): Errs = Nil//TODO
      def _meta(ignored: Seq[Cls]): Cls = Cls.vle(to)
      def atoms(ignored: NodeMetaContext): Atoms = _SquashUUnsafe(f).as.seq }

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
    def atoms(ctx: NodeMetaContext): Atoms = _Size.as.seq }

  // ---------------------------------------------------------------------------
  case class GrabZ(from: TtqKPath1 /* does not specify the surrounding Seq */) extends ActionZV {
    def vldt (in: Seq[Cls]): Errs = from.vldtAsOrigin(in.force.one) //TODO: more
    def _meta(ignored: Seq[Cls]): Cls = Cls.vles(from.node)

    def atoms(ctx: NodeMetaContext): Atoms =
      ctx.afferents.force.one
        .thn(from.pathPairT)
        .thn { pair =>
          if (pair.optional) _GrabZOpt(pair.path)
          else               _GrabZOne(pair.path)  }
        .as.seq }

  // ---------------------------------------------------------------------------
  case class SquashZUnsafe(to: TypeNode, f: Seq[Obj] => Any) extends ActionZV {
      def vldt (in: Seq[Cls]): Errs = Nil//TODO
      def _meta(ignored: Seq[Cls]): Cls = Cls.vle(to)
      def atoms(ignored: NodeMetaContext): Atoms = _SquashZUnsafe(f).as.seq }

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

}

// ===========================================================================

package gallia.actions

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.domain._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUTransforms._
import gallia.atoms.AtomsAsserts._AssertSameType
import gallia.actions.utils

// ===========================================================================
object ActionsUUGenerates {
  private val ValuePlaceholder: AnyValue = null // when only need meta

  // ===========================================================================
  //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

  case class GenerateWV1a(from: TqKPath, to: KPath, f: _ff11) extends ActionUUb {
      def  vldt (c: Cls) : Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta (c: Cls) : Cls   = c.add(to, from.info(c))
      def atomuus(c: Cls): AtomUUs = Seq(                       
          _Transform1to1 (from.pathPairT(c), to, f),
          _AssertSameType(from.pathPairT(c), to)) }

     // ---------------------------------------------------------------------------
    case class GenerateWV1b(from: TqKPath, to: TKPath, f: _ff11) extends ActionUUc {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls    = to.fieldPair(c).thn(c.add)
        def atomuu(c: Cls): AtomUU = _Transform1to1 (from.pathPairT(c), to.path, f) }
    
    // ===========================================================================    
    case class GenerateWV2a(from: TqKPath2, to: KPath, f: _ff21) extends ActionUUb {
        def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta  (c: Cls): Cls     = c.add(to, from.tq1.resolve(c).thn(c.field).info) // choosing first arbitrarily (210817130604)
        def atomuus(c: Cls): AtomUUs = Seq(
            _Transform2to1 (from    .pathPairT(c), to, f(_, _)), 
            _AssertSameType(from.tq1.pathPairT(c), to)) }

      // ---------------------------------------------------------------------------
      case class GenerateWV2b(from: TqKPath2, to: TtqKPath, f: _ff21) extends ActionUUc {       
          def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
          def _meta (c: Cls): Cls    = c.add(to.tq.resolve(c), to.node.forceNonBObjInfo)
          def atomuu(c: Cls): AtomUU = _Transform2to1(from.pathPairT(c), to.tq.resolve(c), f(_, _)) }

    // ===========================================================================    
    case class GenerateWV3a(from: TqKPath3, to: KPath, f: _ff31) extends ActionUUb {
        def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta  (c: Cls): Cls     = c.add(to, from.tq1.resolve(c).thn(c.field).info) // choosing first arbitrarily (210817130604)
        def atomuus(c: Cls): AtomUUs = Seq(
            _Transform3to1 (from    .pathPairT(c), to, f(_, _, _)), 
            _AssertSameType(from.tq1.pathPairT(c), to)) }

      // ---------------------------------------------------------------------------
      case class GenerateWV3b(from: TqKPath3, to: TtqKPath, f: _ff31) extends ActionUUc {       
          def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
          def _meta (c: Cls): Cls    = c.add(to.tq.resolve(c), to.node.forceNonBObjInfo)
          def atomuu(c: Cls): AtomUU = _Transform3to1(from.pathPairT(c), to.tq.resolve(c), f(_, _, _)) }
      
  // ===========================================================================
  case class Generate1VtoV(from: TtqKPath, to: TKPath, f: _ff11) extends ActionUUc {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls    = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform1to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

    // ---------------------------------------------------------------------------      
    case class Generate2VtoV(from: TtqKPath2, to: TKPath, f: _ff21) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform2to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate3VtoV(from: TtqKPath3, to: TKPath, f: _ff31) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform3to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate4VtoV(from: TtqKPath4, to: TKPath, f: _ff41) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform4to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate5VtoV(from: TtqKPath5, to: TKPath, f: _ff51) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform5to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate6VtoV(from: TtqKPath6, to: TKPath, f: _ff61) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform6to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate7VtoV(from: TtqKPath7, to: TKPath, f: _ff71) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform7to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate8VtoV(from: TtqKPath8, to: TKPath, f: _ff81) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform8to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate9VtoV(from: TtqKPath9, to: TKPath, f: _ff91) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform9to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class Generate10VtoV(from: TtqKPath10, to: TKPath, f: _ffA1) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = c.add(to.fieldPair(c))
        def atomuu(c: Cls): AtomUU = _Transform10to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

    // ===========================================================================
    case class GenerateVto2V(from: TtqKPath, to: TKPaths2, f: _ff12) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to2(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto3V(from: TtqKPath, to: TKPaths3, f: _ff13) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to3(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto4V(from: TtqKPath, to: TKPaths4, f: _ff14) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to4(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto5V(from: TtqKPath, to: TKPaths5, f: _ff15) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to5(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto6V(from: TtqKPath, to: TKPaths6, f: _ff16) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to6(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto7V(from: TtqKPath, to: TKPaths7, f: _ff17) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to7(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto8V(from: TtqKPath, to: TKPaths8, f: _ff18) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to8(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto9V(from: TtqKPath, to: TKPaths9, f: _ff19) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to9(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

      // ---------------------------------------------------------------------------
      case class GenerateVto10V(from: TtqKPath, to: TKPaths10, f: _ff1A) extends ActionUUc {
        def  vldt (c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls   = to.addAll(c)
        def atomuu(c: Cls): AtomUU = _Transform1to10(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) }

  // ===========================================================================
  import ActionsUUTransforms.{checkUInput, checkZInput}

  // ---------------------------------------------------------------------------
  abstract class _Generate(target: TqKPath, newPath: KPath) extends ActionUUc {
      protected val _trnsf: utils.NestedTransform

      // ---------------------------------------------------------------------------
      protected val metaF: Boolean => Cls  => Info // boolean is "isOptional"
      protected val dataF: (Cls, PathPair) => _ff11

      // ---------------------------------------------------------------------------
      def protoValidate(c: Cls): Errs =
        target.vldtAsOrigin(c) ++
        _vldt.fieldAbsence(c, newPath) ++
        target.kpath_(c).thn {
          _trnsf.vldt(c, _) }

      // ---------------------------------------------------------------------------
      def _meta (c: Cls): Cls    = target.pathPairT(c).thn { pair => _trnsf.generateMeta(c, pair.path).thn(metaF(pair.optional)).thn(c.add(newPath, _)) }
      def atomuu(c: Cls): AtomUU = target.pathPairT(c).thn { pair => _Transform1to1(pair, newPath, dataF(c, pair)) }
    }

    // ===========================================================================
    case class GenerateUU(target: TqKPath, newPath: KPath, f: HeadU => HeadU) extends _Generate(target, newPath) {
        protected val _trnsf = utils.NestedTransform.parseUU(f)

        protected val metaF = optional  => Info.from(optional, /* multiple */ false)
        protected val dataF = (c, pair) => _trnsf.uu(c.forceNestedClass(pair.path), pair.optional)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = protoValidate(c) ++ target.checkErrors(c)(checkUInput) }

      // ===========================================================================
      case class GenerateZZ(target: TqKPath, newPath: KPath, f: HeadZ => HeadZ) extends _Generate(target, newPath) {
        protected val _trnsf = utils.NestedTransform.parseZZ(f)

        protected val metaF = optional  => Info.from(optional, /* multiple */ true)
        protected val dataF = (c, pair) => _trnsf.zz(c.forceNestedClass(pair.path), pair.optional)

        // ---------------------------------------------------------------------------
        def  vldt(c: Cls): Errs = protoValidate(c) ++ target.checkErrors(c)(checkZInput) }

}

// ===========================================================================

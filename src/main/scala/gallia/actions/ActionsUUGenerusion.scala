package gallia
package actions

import target._
import FunctionWrappers._
import atoms.AtomsUUTransforms._
import atoms.AtomsAsserts._AssertSameType

// ===========================================================================
object ActionsUUGenerusion { //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

  case class Generate1VtoV(from: TtqKPath, to: TKPath, f: _ff11) extends ActionUUc {
      def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta (c: Cls): Cls    = c.add(to.fieldPair(c))
      def atomuu(c: Cls): AtomUU = _Transform1to1(from.pathPairT(c), to.path, from.wrapc(to, f) ) }

    // ---------------------------------------------------------------------------      
    case class Generate2VtoV(from: TtqKPath2, to: TKPath, f: _ff21) extends ActionUUc {
      def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta (c: Cls): Cls    = c.add(to.fieldPair(c))
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
  case class GenerateWV1a(from: TqKPath, to: KPath, f: _ff11) extends ActionUUb {
      def  vldt (c: Cls) : Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta (c: Cls) : Cls   = c.add(to, from.ofni(c))
      def atomuus(c: Cls): AtomUUs = Seq(                       
          _Transform1to1 (from.pathPairT(c), to, f),
          _AssertSameType(from.pathPairT(c), to)) }

     // ---------------------------------------------------------------------------
    case class GenerateWV1b(from: TqKPath, to: TKPath, f: _ff11) extends ActionUUc {
        def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta (c: Cls): Cls    = to.fieldPair(c).pipe(c.add)
        def atomuu(c: Cls): AtomUU = _Transform1to1 (from.pathPairT(c), to.path, f) }
    
    // ===========================================================================    
    case class GenerateWV2a(from: TqKPath2, to: KPath, f: _ff21) extends ActionUUb {
        def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta  (c: Cls): Cls     = c.add(to, from.tq1.resolve(c).pipe(c.field).ofni) // choosing first arbitrarily (210817130604)
        def atomuus(c: Cls): AtomUUs = Seq(
            _Transform2to1 (from    .pathPairT(c), to, f(_, _)), 
            _AssertSameType(from.tq1.pathPairT(c), to)) }

      // ---------------------------------------------------------------------------
      case class GenerateWV2b(from: TqKPath2, to: TtqKPath, f: _ff21) extends ActionUUc {       
          def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
          def _meta (c: Cls): Cls    = c.add(to.tq.resolve(c), to.node.forceNonBObjOfni)
          def atomuu(c: Cls): AtomUU = _Transform2to1(from.pathPairT(c), to.tq.resolve(c), f(_, _)) }

    // ===========================================================================    
    case class GenerateWV3a(from: TqKPath3, to: KPath, f: _ff31) extends ActionUUb {
        def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
        def _meta  (c: Cls): Cls     = c.add(to, from.tq1.resolve(c).pipe(c.field).ofni) // choosing first arbitrarily (210817130604)
        def atomuus(c: Cls): AtomUUs = Seq(
            _Transform3to1 (from    .pathPairT(c), to, f(_, _, _)), 
            _AssertSameType(from.tq1.pathPairT(c), to)) }

      // ---------------------------------------------------------------------------
      case class GenerateWV3b(from: TqKPath3, to: TtqKPath, f: _ff31) extends ActionUUc {       
          def  vldt (c: Cls): Errs   = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
          def _meta (c: Cls): Cls    = c.add(to.tq.resolve(c), to.node.forceNonBObjOfni)
          def atomuu(c: Cls): AtomUU = _Transform3to1(from.pathPairT(c), to.tq.resolve(c), f(_, _, _)) }
      
}

// ===========================================================================

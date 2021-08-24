package gallia.actions

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUTransforms._
import gallia.atoms.AtomsAsserts._AssertSameType

// ===========================================================================
object ActionsUUFusion { import utils.ActionsUtils.removeAll
  //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

  // ===========================================================================
  case class Fuse2(from: TtqKPath2, to: TtqKPath, f: _ff21) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil //FIXME: ensure some fields left + no whatevers
      def _meta(c: Cls): Cls   = to.puts1(c).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform2to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.resolve(c)) }

    // ===========================================================================
    case class Fuse3(from: TtqKPath3, to: TtqKPath, f: _ff31) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform3to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse4(from: TtqKPath4, to: TtqKPath, f: _ff41) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform4to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse5(from: TtqKPath5, to: TtqKPath, f: _ff51) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform5to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse6(from: TtqKPath6, to: TtqKPath, f: _ff61) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform6to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse7(from: TtqKPath7, to: TtqKPath, f: _ff71) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform7to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse8(from: TtqKPath8, to: TtqKPath, f: _ff81) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform8to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removeAll(from.__kpathz(c)) }

  // ===========================================================================    
  case class FusionWV2a(from: TqKPath2, to: KPath, f: _ff21) extends ActionUUb {
      def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = {
        val fromPath1 = from.tq1.resolve(c)
        val fromPath2 = from.tq2.resolve(c)

        c .add(to, fromPath1.thn(c.field).info) // choosing first arbitrarily (210817130604)
          .remove (fromPath1)
          .remove (fromPath2)
      }

      // ---------------------------------------------------------------------------
      def atomuus(c: Cls): AtomUUs = {
        val fromPathPair = from.pathPairT(c)

        Seq(
            _Transform2to1 (fromPathPair,       to, f),           
            _AssertSameType(fromPathPair.pair1, to)) ++ // choosing first arbitrarily (210817130604)
        removeAll(from.__kpathz(c))
      }
    }

    // ---------------------------------------------------------------------------    
    case class FusionWV3a(from: TqKPath3, to: KPath, f: _ff31) extends ActionUUb {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
  
        // ---------------------------------------------------------------------------
        def _meta(c: Cls): Cls = {
          val fromPath1 = from.tq1.resolve(c)
          val fromPath2 = from.tq2.resolve(c)
          val fromPath3 = from.tq3.resolve(c)
  
          c .add(to, fromPath1.thn(c.field).info) // choosing first arbitrarily (210817130604)
            .remove (fromPath1)
            .remove (fromPath2)
            .remove (fromPath3)
        }
  
        // ---------------------------------------------------------------------------
        def atomuus(c: Cls): AtomUUs = {
          val fromPathPair = from.pathPairT(c)
  
          Seq(
              _Transform3to1 (fromPathPair,       to, f),           
              _AssertSameType(fromPathPair.pair1, to)) ++ // choosing first arbitrarily (210817130604)
          removeAll(from.__kpathz(c))
        }
      }  
      
  // ===========================================================================
  case class FusionWV2b(from: TqKPath2, to: KPath, toType: TypeNode, f: _ff21) extends ActionUUb {       
      def vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c) ++ _vldt.validType(toType)

      def _meta(c: Cls): Cls    =          
        c .add   (to, toType.forceNonBObjInfo)
          .remove(from.tq1.resolve(c))
          .remove(from.tq2.resolve(c))

      def atomuus(c: Cls): AtomUUs =
        _Transform2to1(from.pathPairT(c), to, f) +: // choosing first arbitrarily (210817130604)
        removeAll(from.__kpathz(c))                          
    }
    
    // ---------------------------------------------------------------------------
    case class FusionWV3b(from: TqKPath3, to: KPath, toType: TypeNode, f: _ff31) extends ActionUUb {       
      def vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c) ++ _vldt.validType(toType)

      def _meta(c: Cls): Cls    =          
        c .add   (to, toType.forceNonBObjInfo)
          .remove(from.tq1.resolve(c))
          .remove(from.tq2.resolve(c))
          .remove(from.tq3.resolve(c))

      def atomuus(c: Cls): AtomUUs =
        _Transform3to1(from.pathPairT(c), to, f) +: // choosing first arbitrarily (210817130604)
        removeAll(from.__kpathz(c))                          
    }    

}

// ===========================================================================

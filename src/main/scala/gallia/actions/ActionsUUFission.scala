package gallia.actions

import gallia._
import gallia.target._
import gallia.domain._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUTransforms._
import gallia.atoms.AtomsAsserts._AssertSameType
import gallia.actions.utils.ActionsUtils

// ===========================================================================
object ActionsUUFission { import ActionsUtils.{remove, removeAll}
  //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

  // ===========================================================================
  case class Fission2(from: TtqKPath, to: TKPaths2, f: _ff12) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to2(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission3(from: TtqKPath, to: TKPaths3, f: _ff13) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to3(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission4(from: TtqKPath, to: TKPaths4, f: _ff14) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to4(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission5(from: TtqKPath, to: TKPaths5, f: _ff15) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to5(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission6(from: TtqKPath, to: TKPaths6, f: _ff16) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to6(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission7(from: TtqKPath, to: TKPaths7, f: _ff17) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to7(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }

    // ---------------------------------------------------------------------------
    case class Fission8(from: TtqKPath, to: TKPaths8, f: _ff18) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to8(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission9(from: TtqKPath, to: TKPaths9, f: _ff19) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to9(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission10(from: TtqKPath, to: TKPaths10, f: _ff1A) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)
      def _meta(c: Cls): Cls   = to.addAll(c).remove(from.kpath_(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to10(from.pathPairT(c), to.kpathT, from.wrapc(to, f) ) +:
        remove(from.kpathT(c)) }
    
  // ===========================================================================    
  case class FissionWV2a(from: TqKPath, to: KPaths2, f: _ff12) extends ActionUUb {
      def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls = {
        val fromPath = from.resolve(c)
        val info     = c.field(fromPath).info

        to.paths.foldLeft(c) { _.add(_, info) }
          .remove(fromPath)
      }
      
      // ---------------------------------------------------------------------------
      def atomuus(c: Cls): AtomUUs = {
        val fromPathPair = from. pathPairT(c)
        
        Seq(_Transform1to2(fromPathPair, to, f)) ++           
        to.paths.map { toPath => _AssertSameType(fromPathPair, toPath) } ++
        removeAll(from.__kpathz(c))
      }
    }

    // ===========================================================================
    case class FissionWV3a(from: TqKPath, to: KPaths3, f: _ff13) extends ActionUUb {
        def  vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)

        // ---------------------------------------------------------------------------
        def _meta  (c: Cls): Cls = {
          val fromPath = from.resolve(c)
          val info     = c.field(fromPath).info
  
          to.paths.foldLeft(c) { _.add(_, info) }
            .remove(fromPath)
        }
      
        // ---------------------------------------------------------------------------
        def atomuus(c: Cls): AtomUUs = {
          val fromPathPair = from. pathPairT(c)

          Seq(_Transform1to3(fromPathPair, to, f)) ++           
          to.paths.map { toPath => _AssertSameType(fromPathPair, toPath) } ++
          removeAll(from.__kpathz(c))
        }        
    }
    
    // ===========================================================================
    case class FissionWV2b(from: TqKPath, to: TKPaths2, f: _ff12) extends ActionUUb {       
        def vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)

        // ---------------------------------------------------------------------------
        def _meta(c: Cls): Cls    =  
          to
            .values.map(_.fieldPair(c)).foldLeft(c)(_ add _)
            .remove(from.resolve(c))

        def atomuus(c: Cls): AtomUUs = Seq(               
          _Transform1to2(from.pathPairT(c), to.kpathT, f )) ++ 
          removeAll(from.__kpathz(c))          
    }

    // ---------------------------------------------------------------------------
    case class FissionWV3b(from: TqKPath, to: TKPaths3, f: _ff13) extends ActionUUb {       
        def vldt(c: Cls): Errs = from.vldtAsOrigin(c) ++ to.vldtAsNewDestination(c)

        // ---------------------------------------------------------------------------
        def _meta(c: Cls): Cls    =          
          to
            .values.map(_.fieldPair(c)).foldLeft(c)(_ add _)
            .remove(from.resolve(c))

        // ---------------------------------------------------------------------------
        def atomuus(c: Cls): AtomUUs = Seq(               
          _Transform1to3(from.pathPairT(c), to.kpathT, f )) ++ 
          removeAll(from.__kpathz(c))
    }

}

// ===========================================================================

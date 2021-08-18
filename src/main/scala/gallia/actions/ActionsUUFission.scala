package gallia.actions

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUTransforms._

// ===========================================================================
object ActionsUUFission { import ActionsUUFuse.removals0
  //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

    // ===========================================================================    
    case class FissionWV2a(from: TqKPath, to: TqKPath2, f: _ff12) extends ActionUUb {
        def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c)
//FIXME: vld new dest

        def _meta  (c: Cls): Cls     = {
          val fromPath = from.resolve(c)
          val info     = c.field(fromPath).info

          c .add(to.tq1.resolve(c), info)
            .add(to.tq2.resolve(c), info)
            .remove(fromPath)
        }
        
        def atomuus(c: Cls): AtomUUs = {
          Seq(               
              _Transform1to2(from.pathPairT(c), to.kpathT(c), f )) ++ 
              removals0(from.__kpathz(c)) }
//FIXME        
//_AssertSameType(from.tq1.pathPairT(c), to)
          }

      // ---------------------------------------------------------------------------
      case class FissionWV3a(from: TqKPath, to: TqKPath3, f: _ff13) extends ActionUUb {
          def  vldt  (c: Cls): Errs    = from.vldtAsOrigin(c)
  //FIXME: vld new dest
  
          def _meta  (c: Cls): Cls     = {
            val fromPath = from.resolve(c)
            val info     = c.field(fromPath).info
  
            c .add(to.tq1.resolve(c), info)
              .add(to.tq2.resolve(c), info)
              .add(to.tq3.resolve(c), info)
              .remove(fromPath)
          }
          
          def atomuus(c: Cls): AtomUUs = {
            Seq(               
                _Transform1to3(from.pathPairT(c), to.kpathT(c), f )) ++ 
                removals0(from.__kpathz(c)) }
  //FIXME        
  //_AssertSameType(from.tq1.pathPairT(c), to)
            }

    // ===========================================================================
    case class FissionWV2b(from: TqKPath, to: TtqKPath2, f: _ff12) extends ActionUUb {       
          def  vldt (c: Cls): Errs   =
Nil//from.vldtAsOrigin(c).orIfEmpty { to.vldtAsNewDestination(c) }

          def _meta (c: Cls): Cls    =          
            c .add   (to.ttq1.fieldPair(c))
              .add   (to.ttq2.fieldPair(c))
              .remove(from.resolve(c))

          def atomuus(c: Cls): AtomUUs = Seq(               
            _Transform1to2(from.pathPairT(c), to.kpathT(c), f )) ++ 
            removals0(from.__kpathz(c))          
      }

      // ---------------------------------------------------------------------------
      case class FissionWV3b(from: TqKPath, to: TtqKPath3, f: _ff13) extends ActionUUb {       
          def  vldt (c: Cls): Errs   =
Nil//from.vldtAsOrigin(c).orIfEmpty { to.vldtAsNewDestination(c) }

          def _meta (c: Cls): Cls    =          
            c .add   (to.ttq1.fieldPair(c))
              .add   (to.ttq2.fieldPair(c))
              .add   (to.ttq3.fieldPair(c))
              .remove(from.resolve(c))

          def atomuus(c: Cls): AtomUUs = Seq(               
            _Transform1to3(from.pathPairT(c), to.kpathT(c), f )) ++ 
            removals0(from.__kpathz(c))          
      }
      
  // ===========================================================================
  case class Fission2(from: TtqKPath, to: TtqKPath2, f: _ff12) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to2(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission3(from: TtqKPath, to: TtqKPath3, f: _ff13) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to3(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission4(from: TtqKPath, to: TtqKPath4, f: _ff14) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to4(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission5(from: TtqKPath, to: TtqKPath5, f: _ff15) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to5(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission6(from: TtqKPath, to: TtqKPath6, f: _ff16) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to6(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission7(from: TtqKPath, to: TtqKPath7, f: _ff17) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to7(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission8(from: TtqKPath, to: TtqKPath8, f: _ff18) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to8(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission9(from: TtqKPath, to: TtqKPath9, f: _ff19) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to9(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission10(from: TtqKPath, to: TtqKPath10, f: _ff1A) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = 
        _Transform1to10(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +:
        removals0(from.__kpathz(c)) }    
}

// ===========================================================================

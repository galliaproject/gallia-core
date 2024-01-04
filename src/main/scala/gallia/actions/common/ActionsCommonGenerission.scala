package gallia
package actions
package common

import trgt._
import domain._
import FunctionWrappers._
import atoms.common.AtomsCommonTransforms._

// ===========================================================================
object ActionsCommonGenerission { //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

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

}

// ===========================================================================

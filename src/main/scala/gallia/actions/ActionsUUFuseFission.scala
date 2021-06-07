package gallia.actions

import aptus.Anything_

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsUUVeryBasics._
import gallia.atoms.AtomsUUTransforms._

// ===========================================================================
object ActionsUUFuseFission {

  @deprecated private def removals0(values: KPathz): Seq[AtomUU] = // TODO: t210115175745
      values.toSeq.map(potentiallyNested(_Remove))

    // ---------------------------------------------------------------------------
    private def potentiallyNested(nestee: Key => AtomUU)(path: KPath): AtomUU =
      path.initPair match {
        case (None      , leaf) =>                        nestee(leaf)
        case (Some(tail), leaf) => _Nested(parent = tail, nestee(leaf)) }

  // ===========================================================================
  //TODO: t210111095156 separate all the Whatever and t210111095157 case-class versions...

  case class Fuse2(from: TtqKPath2, to: TtqKPath, f: _ff21) extends ActionUUb {
      def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil //FIXME: ensure some fields left + no whatevers
      def _meta(c: Cls): Cls   = to.puts1(c).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform2to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

  // ---------------------------------------------------------------------------
  case class Fuse2WV(from: TqKPath2, toEither: Either[KPath, TtqKPath], multiple: Boolean, f: _ff21) extends ActionUUb { // TODO: split up WV1 vs WV2...
      def  vldt(c: Cls): Errs  = from.doVldtTargetQueries(c) ++ Nil //FIXME: ensure some fields left
        // TODO: t210111091636 - validate all are whatever and of the same (or compatible) type

      // ---------------------------------------------------------------------------
      def _meta(c: Cls): Cls   = {
        val froms = from.pathz(c)

        val field1 = c.field(froms.head /* since all same type */)

        c .put(
            toEither.fold(identity, _.tq.resolve(c)),
              Info(
                  field1.thnIf(multiple)(_.toMultiple).info.container,
                  toEither match {
                    case Left (_) => field1.info.containee
                    case Right(to) => to.node.forceNonBObjInfo.containee }))
          .remove(froms)
      }

      // ---------------------------------------------------------------------------
      def atomuus(c: Cls): AtomUUs = _Transform2to1(from.pathPairT(c), toEither.fold(identity, _.kpathT(c)), f ) +: removals0(from.pathz(c))
    }

    // ===========================================================================
    case class Fuse3(from: TtqKPath3, to: TtqKPath, f: _ff31) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform3to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse4(from: TtqKPath4, to: TtqKPath, f: _ff41) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform4to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse5(from: TtqKPath5, to: TtqKPath, f: _ff51) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform5to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse6(from: TtqKPath6, to: TtqKPath, f: _ff61) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform6to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse7(from: TtqKPath7, to: TtqKPath, f: _ff71) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform7to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

      // ---------------------------------------------------------------------------
      case class Fuse8(from: TtqKPath8, to: TtqKPath, f: _ff81) extends ActionUUb {
        def  vldt(c: Cls): Errs  = from.vldtAsOrigin(c) ++ Nil//TODO
        def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
        def atomuus(c: Cls): AtomUUs = _Transform8to1(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

  // ===========================================================================
  case class Fission2(from: TtqKPath, to: TtqKPath2, f: _ff12) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to2(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission3(from: TtqKPath, to: TtqKPath3, f: _ff13) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to3(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission4(from: TtqKPath, to: TtqKPath4, f: _ff14) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to4(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission5(from: TtqKPath, to: TtqKPath5, f: _ff15) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to5(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission6(from: TtqKPath, to: TtqKPath6, f: _ff16) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to6(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission7(from: TtqKPath, to: TtqKPath7, f: _ff17) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to7(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }

    // ---------------------------------------------------------------------------
    case class Fission8(from: TtqKPath, to: TtqKPath8, f: _ff18) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to8(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission9(from: TtqKPath, to: TtqKPath9, f: _ff19) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to9(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }
    
    // ---------------------------------------------------------------------------
    case class Fission10(from: TtqKPath, to: TtqKPath10, f: _ff1A) extends ActionUUb {
      def  vldt(c: Cls): Errs  = Nil// TODO
      def _meta(c: Cls): Cls   = to.puts0(c, from.containees(c)).remove(from.__kpathz(c))
      def atomuus(c: Cls): AtomUUs = _Transform1to10(from.pathPairT(c), to.kpathT(c), from.wrapc(to, f) ) +: removals0(from.__kpathz(c)) }    
}

// ===========================================================================

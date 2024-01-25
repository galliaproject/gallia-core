package gallia
package actions

import trgt._
import FunctionWrappers._
import atoms.AtomsZZSorting._
import atoms.AtomsOthers._IdentityZZ
import atoms.utils.SuperMetaPair
import atoms.utils.SortWrapping._
import domain.{Sorter, SortingPair}

// ===========================================================================
object ActionsZZSorting {

  // ---------------------------------------------------------------------------
  case class SortUnsafe[T: WTT](f: Obj => T, ord: Ordering[T]) extends ActionZZ11 with IdentityVM1 {
    //TODO: at least check T?
    def  atomzz(c: Cls) = _SortUnsafe(f: Obj => T, SuperMetaPair(implicitly[WTT[T]].ctag, ord)) }

  // ===========================================================================
  case class SortByAll(pair: SortingPair) extends ActionZZ11 with IdentityVM1 {
      def  atomzz(c: Cls) = _SortByAll(c, pair) }

  // ===========================================================================
  case class SortBy1(sorter: Sorter) extends ActionZZ11 with IdentityVM1 {
    def atomzz(c: Cls) = sortBy1(c)(sorter) } //TODO: check target (use HasTypedTargetQueryXXX?)

  // ---------------------------------------------------------------------------
  case class SortBy2(sorter1: Sorter, sorter2: Sorter) extends ActionZZ11 with IdentityVM1 {
    def  atomzz(c: Cls) = sortBy2(c)(sorter1, sorter2) } // TODO: also check targets are distinct

  // ---------------------------------------------------------------------------
  case class SortBy3(sorter1: Sorter, sorter2: Sorter, sorter3: Sorter) extends ActionZZ11 with IdentityVM1 {
    def  atomzz(c: Cls) = sortBy3(c)(sorter1, sorter2, sorter3) } // TODO: also check targets are distinct

  // ---------------------------------------------------------------------------
  case class SortByN(targets: TqKPathz) extends ActionZZ11 with IdentityVM1 {
     // TODO: validate 5 at most (and at least one?)
    def  atomzz(c: Cls) =
      targets.resolve(c).values match {
        case Nil                     => _IdentityZZ
        case Seq(p1)                 => sortBy1(c)(Sorter.default(p1))
        case Seq(p1, p2)             => sortBy2(c)(Sorter.default(p1), Sorter.default(p2))
        case Seq(p1, p2, p3)         => sortBy3(c)(Sorter.default(p1), Sorter.default(p2), Sorter.default(p3))
        case Seq(p1, p2, p3, p4)     => ??? // TODO
        case Seq(p1, p2, p3, p4, p5) => ??? // TODO
        case x => ??? } }//TODO

  // ===========================================================================
  private def sortBy1(c: Cls)(sorter : Sorter)                                   = _SortBy1(c, SortWrapper1.from(c)(sorter))
  private def sortBy2(c: Cls)(sorter1: Sorter, sorter2: Sorter)                  = _SortBy2(c, SortWrapper2.from(c)(sorter1, sorter2))
  private def sortBy3(c: Cls)(sorter1: Sorter, sorter2: Sorter, sorter3: Sorter) = _SortBy3(c, SortWrapper3.from(c)(sorter1, sorter2, sorter3))

  // ===========================================================================
  //TODO: check targets (use HasTypedTargetQueryXXX?)

  case class CustomSort1[D](target: TtqKPath, f: _ff11, meta: SuperMetaPair[D]) extends ActionZZ11 with IdentityVM1 {
      def  atomzz(c: Cls) = _CustomSort1(target.pathPairT(c), f, meta) }

    // ---------------------------------------------------------------------------
    case class CustomSort2[D](target: TtqKPath2, f: _ff21, meta: SuperMetaPair[D]) extends ActionZZ11 with IdentityVM1 {
      def  atomzz(c: Cls) = _CustomSort2(target.pathPairT(c), f, meta) }

}

// ===========================================================================


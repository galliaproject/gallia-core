package gallia.actions

import gallia._
import gallia.target._
import gallia.FunctionWrappers._
import gallia.atoms.AtomsZZSorting._
import gallia.atoms.AtomsOthers._IdentityZZ
import gallia.atoms.utils.SuperMetaPair
import gallia.domain.{Sorter, SortingPair}

// ===========================================================================
object ActionsZZSorting {

  // ---------------------------------------------------------------------------
  case class SortUnsafe[T: ClassTag](f: Obj => T, ord: Ordering[T]) extends ActionZZc with IdentityVM1 {
    //TODO: at least check T?
    def  atomzz(c: Cls) = _SortUnsafe(f: Obj => T, classTag[T], ord: Ordering[T]) }

  // ===========================================================================
  case class SortByAll(pair: SortingPair) extends ActionZZc with IdentityVM1 {
      def  atomzz(c: Cls) = _SortByAll(c, pair) }

  // ===========================================================================
  case class SortBy1(sorter: Sorter) extends ActionZZc with IdentityVM1 {
    //TODO: check target (use HasTypedTargetQueryXXX?)
    def atomzz(c: Cls) = sortBy1(c)(sorter) }

  // ---------------------------------------------------------------------------
  case class SortBy2[T](sorter1: Sorter, sorter2: Sorter) extends ActionZZc with IdentityVM1 {
    // TODO: also check targets are distinct
    def  atomzz(c: Cls) = sortBy2(c)(sorter1, sorter2) }

  // ---------------------------------------------------------------------------
  case class SortByN(targets: TqKPathz) extends ActionZZc with IdentityVM1 {
     // TODO: validate 5 at most (and at least one?)
    def  atomzz(c: Cls) =
      targets.resolve(c).values match {
        case Nil                     => _IdentityZZ
        case Seq(p1)                 => sortBy1(c)(Sorter.from(p1))
        case Seq(p1, p2)             => sortBy2(c)(Sorter.from(p1), Sorter.from(p2))
        case Seq(p1, p2, p3)         => ???
        case Seq(p1, p2, p3, p4, p5) => ??? // TODO
        case x => ??? } }//TODO

  // ===========================================================================
  //TODO: t210124100009 - 3 different meaning of "pair" here... find better names

  private def sortBy1(c: Cls)(sorter1: Sorter) = {
      val pair1 = sorter1.target.pathPairT(c)
      val spair = SuperMetaPair.parse(c, pair1.path, sorter1.pair)
      _SortBy1(pair1, spair)
    }

    // ---------------------------------------------------------------------------
    private def sortBy2(c: Cls)(sorter1: Sorter, sorter2: Sorter) = {
      val pair1 = sorter1.target.pathPairT(c)
      val pair2 = sorter2.target.pathPairT(c)

      val spair1 = SuperMetaPair.parse(c, pair1.path, sorter1.pair)
      val spair2 = SuperMetaPair.parse(c, pair2.path, sorter2.pair)

      _SortBy2(
          pair1, spair1,
          pair2, spair2)
    }

  // ===========================================================================
  //TODO: check targets (use HasTypedTargetQueryXXX?)

  case class CustomSort1[D](target: TtqKPath, f: _ff11, ctag: ClassTag[D], ord: Ordering[D]) extends ActionZZc with IdentityVM1 {
      def  atomzz(c: Cls) = _CustomSort1(target.pathPairT(c), f, ctag, ord) }

    // ---------------------------------------------------------------------------
    case class CustomSort2[D](target: TtqKPath2, f: _ff21, ctag: ClassTag[D], ord: Ordering[D]) extends ActionZZc with IdentityVM1 {
      def  atomzz(c: Cls) = _CustomSort2(target.pathPairT(c), f, ctag, ord) }

}

// ===========================================================================


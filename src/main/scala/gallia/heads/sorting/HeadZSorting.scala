package gallia.heads.sorting

import scala.reflect.ClassTag

import gallia._
import gallia.FunctionWrappers._
import gallia.actions.ActionsZZSorting._
import gallia.domain.{Sorter, SortingPair}

// ===========================================================================
@Max5
trait HeadZSorting { self: HeadZ =>
  import SEL.SortingSingle  .{resolve => resolve1}
  import SEL.SortingMultiple.{resolve => resolveN}

  // ---------------------------------------------------------------------------
  import TSL.Sorting.resolve
  import TSL.Sorting.resolve2

  // ===========================================================================
  /** may be costly, favor using a subset whenever possible */
  def sort(descending: Boolean = false, missingLast: Boolean = false): Self = zz(SortByAll(SortingPair(descending, missingLast)))

  def sort(pair: SortingPair): Self = zz(SortByAll(pair))

    def sort: Self = sortAscending
      def sortAscending : Self = sort(descending = false)
      def sortDescending: Self = sort(descending = true)

  // ===========================================================================
  // sort by 1

  def sortBy(f: KPathW, descending: Boolean = false, missingLast: Boolean = false): Self =
        zz(SortBy1(Sorter.from(f.value, descending, missingLast)))

      // ---------------------------------------------------------------------------
      def sortBy(s1: Sorter): Self = zz(SortBy1(s1))
        def sortBy(sel: SingleSelection): Self = sortBy(Sorter(resolve1(sel)))
          def sortBy(f: KPathW): Self = sortBy(_.explicit(f))

        // ---------------------------------------------------------------------------
        def sortByAscending(sel: SingleSelection): Self = zz(SortBy1(Sorter(resolve1(sel), descending = false)))
          def sortByAscending(f1: KPathW): Self = sortByAscending(_.explicit(f1.value))

        // ---------------------------------------------------------------------------
        def sortByDescending(sel: SingleSelection): Self = zz(SortBy1(Sorter(resolve1(sel), descending = true)))
          def sortByDescending(f1: KPathW): Self = sortByDescending(_.explicit(f1.value))

    // ---------------------------------------------------------------------------
    // sort by 2
    def sortBy(s1: Sorter, s2: Sorter): Self = zz(SortBy2(s1, s2))
      def sortBy(sel1: SingleSelection, sel2: SingleSelection): Self = sortBy(Sorter(resolve1(sel1)), Sorter(resolve1(sel2)))
        def sortBy(f1: KPathW, f2: KPathW): Self = sortBy(_.explicit(f1), _.explicit(f2))

    // TODO: more

    // ---------------------------------------------------------------------------
    // sort by N
    def sortByAll(sel: MultiSelection): Self = zz(SortByN(resolveN(sel)))
      def sortByAll(paths: KPathWz)      : Self = sortByAll(_.explicit(paths))

  // ===========================================================================
  // safe

  // sort using 1
  def sortUsing[O1: WTT](f1: SortingT[O1]) = new {
      def using[D: WTT: Ordering](f: O1 => D): Self = zz(CustomSort1(resolve(f1), wrap(f), ctag[D], implicitly[Ordering[D]])) }

    // ---------------------------------------------------------------------------
    // sort using 2

    def sortUsing[O1: WTT, O2: WTT](f1: SortingT[O1], f2: SortingT[O2]) = new {
      def using[D: WTT: Ordering](f: (O1, O2) => D): Self =
        zz(CustomSort2(resolve2(f1, f2), wrap21(f), ctag[D], implicitly[Ordering[D]])) }

    // TODO: more

  // ===========================================================================
  // unsafe
  def sortUsingUnsafe[T: Ordering : ClassTag](f: Obj => T): Self = zz(SortUnsafe(f, implicitly[Ordering[T]]))

    def sortUsingUnsafe(ord: Ordering[Obj]): Self = { implicit val x = ord; sortUsingUnsafe(o => o) }
    def sortUsingUnsafe(        f:       (Obj, Obj)  => Int): Self = sortUsingUnsafe(new Ordering[Obj] { def compare(x: Obj, y: Obj): Int = f(    x, y)  })
    def sortUsingUnsafe(c: Cls)(f: (Cls, (Obj, Obj)) => Int): Self = sortUsingUnsafe(new Ordering[Obj] { def compare(x: Obj, y: Obj): Int = f(c, (x, y)) })

  // ===========================================================================
  def sortByFirstKey: Self = sortByAscendingFirstKey
  def sortByLastKey : Self = sortByAscendingLastKey

    // ---------------------------------------------------------------------------
    def sortByAscendingFirstKey: Self = sortByAscending(_.firstKey)
    def sortByAscendingLastKey : Self = sortByAscending(_.lastKey)

    def sortByDescendingFirstKey: Self = sortByDescending(_.firstKey)
    def sortByDescendingLastKey : Self = sortByDescending(_.lastKey)

  // ---------------------------------------------------------------------------
  def sortByCount: Self = sortByAscendingCount
    def sortByAscendingCount : Self = sortBy(Sorter.from(_count, descending = false))
    def sortByDescendingCount: Self = sortBy(Sorter.from(_count, descending = true ))

  def sortByGroupSize: Self = sortByAscendingGroupSize
    def sortByAscendingGroupSize : Self = ???//TODO: sortBy(Sorter.from(_count, descending = false))
    def sortByDescendingGroupSize: Self = ???//TODO: sortBy(Sorter.from(_count, descending = true ))
}

// ===========================================================================


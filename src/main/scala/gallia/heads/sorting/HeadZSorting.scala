package gallia
package heads
package sorting

import FunctionWrappers._
import actions.ActionsZZSorting._
import domain.{Sorter, SortingPair}
import atoms.utils.SuperMetaPair

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
  def sort(descending: Boolean, missingLast: Boolean): Self = zz(SortByAll(SortingPair(descending, missingLast)))

  def sort(pair: SortingPair): Self = zz(SortByAll(pair))

    def sort: Self = sortAscending
      def sortAscending : Self = sort(descending = false, missingLast = false)
      def sortDescending: Self = sort(descending = true , missingLast = true)

  // ===========================================================================
  // sort by 1

  def sortBy(f: KPathW): Self =
          zz(SortBy1(Sorter.from(f.value, descending = false, missingLast = false)))

        def sortBy(f: KPathW, descending: Boolean): Self =
          zz(SortBy1(Sorter.from(f.value, descending, missingLast = descending /* match behavior */)))

        def sortBy(f: KPathW, descending: Boolean, missingLast: Boolean): Self =
         zz(SortBy1(Sorter.from(f.value, descending, missingLast)))

      // ---------------------------------------------------------------------------
      def sortBy(s1: Sorter): Self = zz(SortBy1(s1))
        def sortBy(sel: SingleSelection): Self = sortBy(Sorter(resolve1(sel), descending = false, missingLast = false))

        // ---------------------------------------------------------------------------
        def sortByAscending(sel: SingleSelection): Self = zz(SortBy1(Sorter(resolve1(sel), descending = false, missingLast = false)))
          def sortByAscending(f1: KPathW): Self = sortByAscending(_.explicit(f1.value))

        // ---------------------------------------------------------------------------
        def sortByDescending(sel: SingleSelection): Self = zz(SortBy1(Sorter(resolve1(sel), descending = true, missingLast = true)))
          def sortByDescending(f1: KPathW): Self = sortByDescending(_.explicit(f1.value))

    // ---------------------------------------------------------------------------
    // sort by 2
    def sortBy(s1: Sorter, s2: Sorter): Self = zz(SortBy2(s1, s2))
      def sortBy(sel1: SingleSelection, sel2: SingleSelection): Self = sortBy(
          Sorter(resolve1(sel1), descending = false, missingLast = false),
          Sorter(resolve1(sel2), descending = false, missingLast = false))
        def sortBy(f1: KPathW, f2: KPathW): Self = sortBy(_.explicit(f1), _.explicit(f2))

    // ---------------------------------------------------------------------------
    // sort by 3
    def sortBy(s1: Sorter, s2: Sorter, s3: Sorter): Self = zz(SortBy3(s1, s2, s3))
      def sortBy(sel1: SingleSelection, sel2: SingleSelection, sel3: SingleSelection): Self = sortBy(
          Sorter(resolve1(sel1), descending = false, missingLast = false),
          Sorter(resolve1(sel2), descending = false, missingLast = false),
          Sorter(resolve1(sel3), descending = false, missingLast = false))
        def sortBy(f1: KPathW, f2: KPathW, f3: KPathW): Self = sortBy(_.explicit(f1), _.explicit(f2), _.explicit(f3))

    // ---------------------------------------------------------------------------
    // TODO: more

    // ---------------------------------------------------------------------------
    // sort by N
    def sortByAll(sel: MultiSelection): Self = zz(SortByN(resolveN(sel)))
      def sortByAll(paths: KPathWz)      : Self = sortByAll(_.explicit(paths))

  // ===========================================================================
  // safe

  // sort using 1
  def sortUsing[O1: WTT](f1: SortingT[O1]) = new {
      def using[D: WTT: Ordering](f: O1 => D): Self = zz(CustomSort1(resolve(f1), wrap(f), SuperMetaPair(reflect.low.ctag[D], implicitly[Ordering[D]]))) }

    // ---------------------------------------------------------------------------
    // sort using 2

    def sortUsing[O1: WTT, O2: WTT](f1: SortingT[O1], f2: SortingT[O2]) = new {
      def using[D: WTT: Ordering](f: (O1, O2) => D): Self =
        zz(CustomSort2(resolve2(f1, f2), wrap21(f), SuperMetaPair(reflect.low.ctag[D], implicitly[Ordering[D]]))) }

    // TODO: more

  // ===========================================================================
  // unsafe
  def sortUsingUnsafe[T: Ordering : CT](f: Obj => T): Self = zz(SortUnsafe(f, implicitly[Ordering[T]]))

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
    def sortByAscendingCount : Self = sortBy(Sorter.from(_count_all, descending = false, missingLast = false))
    def sortByDescendingCount: Self = sortBy(Sorter.from(_count_all, descending = true , missingLast = true))

  def sortByGroupSize: Self = sortByAscendingGroupSize
    def sortByAscendingGroupSize : Self = ???//TODO: sortBy(Sorter.from(_count, descending = false))
    def sortByDescendingGroupSize: Self = ???//TODO: sortBy(Sorter.from(_count, descending = true ))
}

// ===========================================================================


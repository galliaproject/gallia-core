package gallia
package domain

import target.utils.TargetQueryUtils.tqkpath
import target.TqKPath

// ===========================================================================
case class Sorter(
      target     : TqKPath,
      descending : Boolean = false,
      missingLast: Boolean = false) {
    def pair = SortingPair(descending, missingLast)
  }

  // ===========================================================================
  object Sorter {
    implicit def to(target:  Key ): Sorter = from(target)
    implicit def to(target: SKey ): Sorter = from(target)
    implicit def to(target: EKey ): Sorter = from(target)
    implicit def to(target: UKey ): Sorter = from(target)
    implicit def to(target: KPath): Sorter = from(target)
    implicit def to(target: KPathW): Sorter = from(target)

    // ---------------------------------------------------------------------------
    def from(
        target     : KPathW,
        descending : Boolean = false,
        missingLast: Boolean = false)
      : Sorter =
        Sorter(
            tqkpath(target.value),
            descending,
            missingLast)

    // ---------------------------------------------------------------------------
    trait Sorter__ {
      protected def _path: KPathW

      // ---------------------------------------------------------------------------
      def sorter(
              descending : Boolean = false,
              missingLast: Boolean = false)
            : Sorter =
          Sorter.from(_path, descending, missingLast)

      def asc : Sorter = useAscending
      def desc: Sorter = useDescending

        def useAscending : Sorter = sorter(descending = false)
        def useDescending: Sorter = sorter(descending = true)

      def useMissingFirst: Sorter = sorter(missingLast = false)
      def useMissingLast : Sorter = sorter(missingLast = true)

      def useDescendingAndMissingLast = sorter(descending = true, missingLast = true)
    }
  }

// ===========================================================================
case class SortingPair(
    descending : Boolean,
    missingLast: Boolean)

  // ---------------------------------------------------------------------------
  object SortingPair {
    val Default = SortingPair(false, false)
  }

// ===========================================================================

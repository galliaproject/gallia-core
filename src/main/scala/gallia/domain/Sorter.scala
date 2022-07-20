package gallia
package domain

import aptus.Anything_
import target.utils.TargetQueryUtils.tqkpath
import target.TqKPath
import atoms.utils.SortWrapping.SortWrapper1
import atoms.utils.SuperMetaPair

// ===========================================================================
case class Sorter(
      target     : TqKPath,
      descending : Boolean,
      missingLast: Boolean) {

    def sortWrapper(c: Cls) = {
      val path = target.resolve(c)
      val info = target.info(c)
      val si = info.subInfo1

      SortWrapper1(
        ori       = domain.PathPair(path, info.optional),
        meta      = SuperMetaPair.parse(c, path, SortingPair(descending, missingLast)),
        multiple  = si.multiple,
        numerical = si.isNumericalType,
        reverse   = descending,
        missingLast)
    }
  }

  // ===========================================================================
  object Sorter {
    implicit def to(target:  Key ) : Sorter = default(target)
    implicit def to(target: SKey ) : Sorter = default(target)
    implicit def to(target: EKey ) : Sorter = default(target)
    implicit def to(target: UKey ) : Sorter = default(target)
    implicit def to(target: KPath) : Sorter = default(target)
    implicit def to(target: KPathW): Sorter = default(target)

    // ---------------------------------------------------------------------------
    def default(target: KPathW) = from(target, descending = false, missingLast = false)

    // ---------------------------------------------------------------------------
    def from(
        target     : KPathW,
        descending : Boolean,
        missingLast: Boolean)
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
      missingLast: Boolean) {

    override def toString: String = formatDefault
      def formatDefault: String =
        s"${if (descending) "DSC" else "ASC"}:${if (missingLast) "L" else "F"}"

    def validSpillingCombo: Boolean =
      (!descending && !missingLast) ||
      ( descending &&  missingLast)

    def descendingSpilling: Boolean = this.assert(_.validSpillingCombo, _.errorMessage).descending

    def errorMessage: String = s"220624173138 - spilling does not support putting blank values at the end unless descending (${formatDefault})"
  }

  // ---------------------------------------------------------------------------
  object SortingPair {
    val Default = SortingPair(false, false)
  }

// ===========================================================================

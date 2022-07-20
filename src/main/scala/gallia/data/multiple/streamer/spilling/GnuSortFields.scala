package gallia
package data
package multiple
package streamer
package spilling

import aptus.Index

// ===========================================================================
case class GnuSortFields(values: Seq[GnuSortField]) {

    override def toString = formatDefault
      def formatDefault: String =
        values.map(_.formatDefault).mkString(",")

    def formatOptions: Seq[String] = values.map(_.formatOption)
  }

  // ---------------------------------------------------------------------------
  object GnuSortFields {
    def default                    : GnuSortFields = default(numerical = false)
    def default(numerical: Boolean): GnuSortFields = GnuSortFields(Seq(GnuSortField(index = 1, numerical, reverse = false)))

    // ---------------------------------------------------------------------------
    def from(values: Seq[SortWrapper]): GnuSortFields =
      GnuSortFields(
        values.zipWithIndex.map { case (x, i) =>
          if (!x.sortingPair.validSpillingCombo)
            aptus.illegalArgument(x.sortingPair.errorMessage)

          GnuSortField(index = i + 1, x.numerical, reverse = x.reverse) })
  }

  // ===========================================================================
  case class GnuSortField(
      index    : Index,
      numerical: Boolean,
      reverse  : Boolean) {

    override def toString = formatDefault
      def formatDefault: String =
        s"${index}${if (numerical) "n" else ""}${if (reverse) "r" else ""}"

    // ---------------------------------------------------------------------------
    def formatOption =
      (numerical, reverse) match {
        case (false, false) => s"-k${index},${index}"
        case (false, true ) => s"-k${index}r,${index}r"
        case (true , false) => s"-k${index}n,${index}n"
        case (true , true ) => s"-k${index}nr,${index}nr" }

  }

// ===========================================================================

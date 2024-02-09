package gallia
package run

import aptus.{String_, Seq_}

// ===========================================================================
sealed trait PotentialResultSchema {
    def successOpt: Option[Cls]
    def errorOpt  : Option[PotentialResultSchema.Errors]

    // ---------------------------------------------------------------------------
    def errors    : Errs    = errorOpt.toSeq.flatMap(_.values) // may be empty
    def isError   : Boolean = errors.nonEmpty }

  // ===========================================================================
  object PotentialResultSchema {
    case object UpstreamError                          extends PotentialResultSchema { def successOpt = None       ; val errorOpt = None  }
    case class  Success(value: Cls)                    extends PotentialResultSchema { def successOpt = Some(value); val errorOpt = None    }
    case class  Errors(values: Errs, origin: CallSite) extends PotentialResultSchema { def successOpt = None       ; val errorOpt = Some(this)
      def formatExceptionMessage: String =
        Seq(
              origin.formatDefault,
              errors.map(_.format).joinln)
          .joinlnln
          .sectionAllOff(2) } }

// ===========================================================================

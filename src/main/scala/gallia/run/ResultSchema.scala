package gallia.run

import aptus.{String_, Seq_}
import gallia._

// ===========================================================================
sealed trait ResultSchema {
    def successOpt: Option[Cls]
    def errorOpt  : Option[ResultSchema.Errors]

    // ---------------------------------------------------------------------------
    def errors    : Errs    = errorOpt.toSeq.flatMap(_.values) // may be empty
    def isError   : Boolean = errors.nonEmpty
  }

  // ===========================================================================
  object ResultSchema {
    case object UpstreamError                          extends ResultSchema { def successOpt = None       ; val errorOpt = None  }
    case class  Success(value: Cls)                    extends ResultSchema { def successOpt = Some(value); val errorOpt = None    }
    case class  Errors(values: Errs, origin: CallSite) extends ResultSchema { def successOpt = None       ; val errorOpt = Some(this)
      def formatExceptionMessage: String =
        Seq(
              origin.formatSuccinct,
              errors.map(_.format).joinln,
              origin.formatSuccinct)
          .joinlnln
          .sectionAllOff(2)
    }

    // ---------------------------------------------------------------------------
    /*
      match {
        case UpstreamError        => ???
        case Errors(values: Errs) => ???
        case Result(value : Cls ) => ???
      }
    */
  }

// ===========================================================================

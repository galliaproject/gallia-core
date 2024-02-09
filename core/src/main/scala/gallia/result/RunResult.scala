package gallia
package result

import aptus.Seq_
import plans.{ActionPlan => Plan}
import run.MetaErrorResult

// ===========================================================================
class RunResult[$SuccessResult <: SuccessResult[$Data], $Data](val resultEither: Either[MetaErrorResult, $SuccessResult]) {

    override def toString: String = formatDefault
      def formatDefault: String =
        resultEither match {
          case Left (errorResult) => s"error:210114174424:TODO:${errorResult}"
          case Right(_)           => "success" }

    // ===========================================================================
    def isSuccess : Boolean = resultEither.isRight
    def isError   : Boolean = resultEither.isLeft

    // ---------------------------------------------------------------------------
    def mapData[T](f: $Data => T): Either[MetaErrorResult, T] = 
      resultEither.map { success => f(success.data) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceErrors: Errs =
      resultEither match {
        case Left (errors)  => errors.allErrors
        case Right(success) => aptus.illegalState("MetaErrors (201016114235)", success) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData1b: $Data =
      resultEither match {
        case Left (errors)  => aptus.illegalState("MetaErrors (201016114128)", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => success.data }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData2[T](f: $SuccessResult => T): T =
      resultEither match {
        case Left (errors)  => aptus.illegalState("MetaErrors (201016114132)", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => f(success) } }

  // ===========================================================================
  case class RunResultM   (override val resultEither: Either[MetaErrorResult, SuccessResultM])    extends RunResult[SuccessResultM,    Plan](resultEither)
  case class RunResultU   (override val resultEither: Either[MetaErrorResult, SuccessResultU])    extends RunResult[SuccessResultU,    Obj] (resultEither)
  case class RunResultZ   (override val resultEither: Either[MetaErrorResult, SuccessResultZ])    extends RunResult[SuccessResultZ,    Objs](resultEither)
  case class RunResultV[T](override val resultEither: Either[MetaErrorResult, SuccessResultV[T]]) extends RunResult[SuccessResultV[T], T]   (resultEither)

// ===========================================================================

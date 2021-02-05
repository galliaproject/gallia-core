package gallia.result

import aptus.Seq_

import gallia._
import gallia.run.ErrorResult

// ===========================================================================
abstract class RunResult[$SuccessResult <: SuccessResult[$Data], $Data](either: Either[ErrorResult, $SuccessResult]) {

    override def toString: String = formatDefault
      def formatDefault: String =
        either match {
          case Left (errorResult) => s"error:210114174424:TODO:${errorResult}"
          case Right(_)           => "success" }

    // ===========================================================================
    def isSuccess : Boolean = either.isRight
    def isError   : Boolean = either.isLeft

    // ---------------------------------------------------------------------------
    def mapData[T](f: $Data => T): Either[ErrorResult, T] = either.map { success => f(success.data) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceErrors: Errs =
      either match {
        case Left (errors)  => errors.allErrors
        case Right(success) => illegal("TODO:201016114235", success) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData1b: $Data =
      either match {
        case Left (errors)  => illegal("TODO:201016114128", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => success.data }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData2[T](f: $SuccessResult => T): T =
      either match {
        case Left (errors)  => illegal("TODO:201016114132", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => f(success) }

  }

  // ===========================================================================
  case class RunResultU   (either: Either[ErrorResult, SuccessResultU   ]) extends RunResult[SuccessResultU   , Obj ](either)
  case class RunResultZ   (either: Either[ErrorResult, SuccessResultZ   ]) extends RunResult[SuccessResultZ   , Objs](either)
  case class RunResultV[T](either: Either[ErrorResult, SuccessResultV[T]]) extends RunResult[SuccessResultV[T], T   ](either)

// ===========================================================================

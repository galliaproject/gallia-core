package gallia.result

import aptus.Seq_

import gallia._

class RunResult[$SuccessResult <: SuccessResult[$Data], $Data](val either: Either[MetaErrorResult, $SuccessResult]) {

    override def toString: String = formatDefault
      def formatDefault: String =
        either match {
          case Left (errorResult) => s"error:210114174424:TODO:${errorResult}"
          case Right(_)           => "success" }

    // ===========================================================================
    def isSuccess : Boolean = either.isRight
    def isError   : Boolean = either.isLeft

    // ---------------------------------------------------------------------------
    def mapData[T](f: $Data => T): Either[MetaErrorResult, T] = either.map { success => f(success.data) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceErrors: Errs =
      either match {
        case Left (errors)  => errors.allErrors
        case Right(success) => aptus.illegalState("TODO:201016114235", success) }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData1b: $Data =
      either match {
        case Left (errors)  => aptus.illegalState("TODO:201016114128", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => success.data }

    // ---------------------------------------------------------------------------
    private[gallia] def forceData2[T](f: $SuccessResult => T): T =
      either match {
        case Left (errors)  => aptus.illegalState("TODO:201016114132", errors.allErrors.#@@, errors.formatDefault)
        case Right(success) => f(success) }

  }

  // ===========================================================================
  case class RunResultM   (override val either: Either[MetaErrorResult, SuccessResultM])    extends RunResult[SuccessResultM,    Plan](either)
  case class RunResultU   (override val either: Either[MetaErrorResult, SuccessResultU])    extends RunResult[SuccessResultU,    Obj] (either)
  case class RunResultZ   (override val either: Either[MetaErrorResult, SuccessResultZ])    extends RunResult[SuccessResultZ,    Objs](either)
  case class RunResultV[T](override val either: Either[MetaErrorResult, SuccessResultV[T]]) extends RunResult[SuccessResultV[T], T]   (either)  

// ===========================================================================

package gallia.testing

import util._
import gallia._

// ===========================================================================
sealed trait TestValue { def isOk: Boolean = this == Ok }

  // ---------------------------------------------------------------------------
  case object Ok                        extends TestValue
  case class  Problem(value: Throwable) extends TestValue

// ===========================================================================
private object TestValue {
  private def problem(message: String): TestValue = Problem(new Throwable(message))

  // ===========================================================================
  def __check(u: HeadEnd, value: Cls)  : TestValue = __check(u, Some(value),   None)
  
  def __check(u: HeadEnd, value: Obj)  : TestValue = __check(u, None,          Some(value))
  def __check(u: HeadEnd, value: Objs) : TestValue = __check(u, None,          Some(value))
  
  def __check(u: HeadEnd, value: AObj) : TestValue = __check(u, Some(value.c), Some(value.o))
  def __check(u: HeadEnd, value: AObjs): TestValue = __check(u, Some(value.c), Some(value.z))

  def __check[T](u: HeadEnd, value: T) : TestValue = __check(u, None, Some(value))

  // ===========================================================================
  def __check[$Data](u: HeadEnd, expC: Option[Cls], expD: Option[$Data]): TestValue =
      util.Try { u.run[$Data]() } match {
        case util.Failure(f)   => problem(f.getMessage)
        case util.Success(res) =>
          res.resultEither match {
            case Left (metaErrorResult)                                   => problem(s"210414125640:${metaErrorResult.formatErrors}")
            case Right(successResult) if (successResult.leavesCount != 1) => problem(s"210414125643:MultipleLeaves")
            case Right(successResult) =>
              val (meta, data0) = successResult.pair
              val data = data0 match {
                 case o: Obj  => o
                 case z: Objs => z._toViewBased
                 case x       => x }

              // ---------------------------------------------------------------------------
                   if (expC.exists(_ != meta)) problem(s"\n\nexpected:\n${expC.get}\n\ngot:\n${meta}\n")
              else if (expD.exists(_ != data)) problem(s"\n\nexpected:\n${expD.get}\n\ngot:\n${data
                   match {
                     case o: Obj  => o.formatDefault
                     case z: Objs => z.formatDefault
                     case x       => x.toString }
                 }\n")
              else                             Ok } }

    // ===========================================================================
    def __checkPredicate(u: HeadEnd, msg: String)(f: Objs => Boolean): TestValue =
      util.Try { u.run[Objs]() } match {
        case util.Failure(f)   => problem(f.getMessage)
        case util.Success(res) =>
          res.resultEither match {
            case Left (metaErrorResult)                                   => problem(s"210414125640:${metaErrorResult.formatErrors}")
            case Right(successResult) if (successResult.leavesCount != 1) => problem(s"210414125643:MultipleLeaves")
            case Right(successResult) =>
              val (_, data) = successResult.pair
              
              if (!f(data)) problem(s"\n\n${msg}:\n\ngot:\n${data}\n")
              else          Ok } }

      
  // ===========================================================================
  def __metaError(end: gallia.heads.HeadEnd, markers: Seq[String]): TestValue =
    Try { end.runMetaOnly().resultEither } match {
      case Failure(metaFailure)                                                                => problem(s"210414113945:MetaFailure:${metaFailure.getMessage}")
      case Success(Right(_))                                                                   => problem( "210414114600:ShouldNotHaveSucceeded")
      case Success(Left(metaErrorResult)) if !metaErrorResult.containsAllErrorMarkers(markers) => problem(s"210414114601:MissingErrorMarkers:${markers}:${metaErrorResult.allErrors}")
      case Success(Left(_))                                                                    => Ok }

  // ===========================================================================
  def __dataError(end: gallia.heads.HeadEnd, markers: Seq[String]): TestValue =
    Try { end.runMetaOnly().resultEither } match {
      case Failure(metaFailure)            => problem(s"210414113945:MetaFailure:${metaFailure.getMessage}")
      case Success(Left (metaErrorResult)) => problem(s"210414114439:MetaError:${metaErrorResult.formatDefault}")
      case Success(Right(metaSuccess))     => ___dataError(plan = metaSuccess.data, markers) }

    // ---------------------------------------------------------------------------
    def ___dataError(plan: ActionPlan, markers: Seq[String]): TestValue =
      Try { plan.atomPlan.naiveRun() } match {
        case util.Success(_)                                                             => problem("210414114600:ShouldNotHaveSucceeded")
        case util.Failure(dataError) if (!markers.forall(dataError.getMessage.contains)) => problem(s"210414114601:MissingErrorMarkers:${dataError.getMessage}")
        case util.Failure(_        )                                                     => Ok }

}

// ===========================================================================

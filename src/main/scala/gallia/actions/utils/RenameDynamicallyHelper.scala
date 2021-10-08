package gallia.actions.utils

import aptus.{String_, Option_}
import gallia._

// ===========================================================================
private[actions] object RenameDynamicallyHelper {
  type Result = Either[(KPath, Throwable), RPath]

  // ===========================================================================
  class Results(results: Seq[Result]) {

    override def toString: String = formatDefault

      def formatDefault: String =        
        results
          .map {
              case Left ((path, throwable)) => s"${path.formatDebug} (${ throwable.getMessage})" 
              case Right(rpath)             => rpath.formatDefault }          
          .mkString("\n")
          .prepend(s"${results.size}:")

    private def either: Either[Seq[(KPath, Throwable)], RPathz] =
      results
        .flatMap(_.left.toOption)
         match {
          case Nil => Right(forceRPathz)
          case seq => Left(seq) }

    // ---------------------------------------------------------------------------
    def validate(throwableHandler: (KPath, Throwable) => Err, pathsHandler: RPathz => Errs): Errs =
      either
         match {
          case Left (throwables) => throwables.map(throwableHandler.tupled)
          case Right(qpathz)     => pathsHandler(qpathz) }

    // ---------------------------------------------------------------------------
    def forceRPathz: RPathz = results.map(_.right.get).pipe(RPathz.apply)
  }

  // ===========================================================================
  object Results {

    def parse(modifier: SKey  => SKey, recursively: Boolean)(c: Cls): Results =
        new Results(parseAll(modifier, recursively)(parent = Nil)(c))

      // ---------------------------------------------------------------------------
      private def parseAll(modifier: SKey  => SKey, recursively: Boolean)(parent: Seq[Key])(c: Cls): Seq[Result] =
        c
          .fields
          .flatMap { field =>
            val path: Seq[Key] = parent :+ field.key

            // ---------------------------------------------------------------------------
            val nesting: Seq[Result] =
              if (!recursively) Nil
              else
                field
                  .nestedClassOpt // do depth first (order will matter)
                  .toSeq.flatMap {
                    parseAll(modifier, recursively)(path) } // recursive call

            // ---------------------------------------------------------------------------
            val current: Option[Result] =
              util.Try(modifier(field.skey)) match {

                case util.Failure(throwable) =>
                  Some(Left((KPath.opt(path).force /* TODO: ok? */, throwable)))

                case util.Success(to) =>
                  if (to != field.skey) Some(Right(RPath.from(path, to.symbol)))
                  else                  None }

            // ---------------------------------------------------------------------------
            nesting ++ current
          }
    }

}

// ===========================================================================

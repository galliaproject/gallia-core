package gallia
package atoms

import aptus.String_
import io.out._

// ===========================================================================
object AtomsXO {
  val DefaultSchemaSuffix = ".schema.json" // TODO: t210115114509 - configurable

  // ===========================================================================
  case class _ForeachOutputU(f: Obj => Unit) extends AtomUO { def naive(o: Obj)  = f(o) }
  case class _ForeachOutputZ(f: Obj => Unit) extends AtomZO { def naive(s: Objs) = s.consumeSelfClosing.foreach(f) }
      
  // ===========================================================================
  abstract class _SchemaOutput(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) {
      def naive() = { urlLike.writeFileContent(uriString.append(suffix))(c.formatPrettyJson /* TODO: t210128103821 - format configurable */) } }

    // ---------------------------------------------------------------------------
    case class _SchemaOutputU(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) extends _SchemaOutput(c, uriString, urlLike, suffix) with AtomUO  {
        def naive(ignored: Obj) = { naive() } }

    // ---------------------------------------------------------------------------
    case class _SchemaOutputZ(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) extends _SchemaOutput(c, uriString, urlLike, suffix) with AtomZO {
        def naive(ignored: Objs) = { naive() } }

  // ===========================================================================
  case class _UrlLikeOutputU(ioType: IoTypeU, uriString: String, urlLike: UrlLike) extends AtomUO {
      def naive(o: Obj) = { ioType.defaultFormat(o).pipe(urlLike.writeFileContent(uriString)) } }

    // ---------------------------------------------------------------------------
    case class _UrlLikeOutputZ(ioType: IoTypeZ, uriString: String, urlLike: UrlLike) extends AtomZO {
      def naive(z: Objs) = { ioType.defaultFormat(z).pipe(urlLike.writeFileLines(uriString)) } }

  // ===========================================================================
  // including JSON

  case class _OtherOutputU(c: Cls, ioType: IoTypeU, outlet: OutletType) extends AtomUO {
      def naive(o: Obj) = { ioType.defaultFormat2(c, o).pipe(outlet.writeLine) } }

    // ---------------------------------------------------------------------------
    case class _OtherOutputZ(ioType: IoTypeZ, outlet: OutletType) extends AtomZO {
      def naive(z: Objs) = { ioType.defaultFormat(z).pipe(outlet.writeLines) } }

  // ===========================================================================
  case class _UrlLikeTableOutput(schema: Cls, uriString : String, urlLike: UrlLike, twc: TableWritingContext) extends AtomZO {
      def naive(z: Objs) = { twc.formatTable(schema)(z).pipe(urlLike.writeFileLines(uriString)) } }

    // ---------------------------------------------------------------------------
    case class _OtherTableOutput(schema: Cls, outlet: OutletType, twc: TableWritingContext) extends AtomZO {
      def naive(z: Objs) = twc.formatTable(schema)(z).pipe(outlet.writeLines) }

  // ===========================================================================
  case class _PrettyRowOutput(schema: Cls, outlet: OutletType, twc: PrettyTableWritingContext) extends AtomUO {
      def naive(o: Obj) = _PrettyTableOutput(schema, outlet, twc).naive(Objs.from(List(o))) }
    
    // ---------------------------------------------------------------------------
    case class _PrettyTableOutput(schema: Cls, outlet: OutletType, twc: PrettyTableWritingContext) extends AtomZO {
      def naive(z: Objs) = { twc.formatTable(schema)(z).pipe(outlet.writeLines) } }

  // ===========================================================================
  // TODO: t220916113454 - separate HeadV[T] from HeadV[Seq[U]]
  case class _NakedValueOutput(
        eitherOpt: Option[Either[aptus.OutputFilePath, StringWriter]],
        f        : Vle => (Multiple, aptus.CloseabledIterator[String]))
      extends AtomVO {

    import aptus.aptutils.FileUtils
    import aptus._

    // ---------------------------------------------------------------------------
    def naive(v: Vle)  = {
      val (multiple, result) = f(v)

      if (multiple) processMultipleValues(result)
      else          processSingleValue   (result.consumeAll.force.one)
    }

    // ---------------------------------------------------------------------------
    def processSingleValue(value: String): Unit =
      eitherOpt match {
        case Some(Left(path)) => value.pipe(CloseabledIterator.fromSole).writeGzipLines(path, 100, None)
        case Some(Right(sw))  => value.tap(sw.write); sw.close()
        case None             => value.tap(println) }

    // ---------------------------------------------------------------------------
    def processMultipleValues(values: aptus.CloseabledIterator[String]): Unit =
      eitherOpt match {
        case Some(Left(path)) => values.writeGzipLines(path, 100, None)
        case Some(Right(sw))  => values.map(_.newline).consume(_.foreach(sw.write)); sw.close()
        case None             => values               .consume(_.foreach(println)) }
  }

}

// ===========================================================================

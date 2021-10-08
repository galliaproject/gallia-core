package gallia.atoms

import aptus.{Anything_, String_}

import gallia._
import gallia.io.out._

// ===========================================================================
object AtomsXO {
  val DefaultSchemaSuffix = ".schema.json" // TODO: t210115114509 - configurable

  // ===========================================================================
  abstract class _SchemaOutput(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) {
      def naive() { urlLike.writeFileContent(uriString.append(suffix))(c.formatPrettyJson /* TODO: t210128103821 - format configurable */) } }

    // ---------------------------------------------------------------------------
    case class _SchemaOutputU(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) extends _SchemaOutput(c, uriString, urlLike, suffix) with AtomUO  {
        def naive(ignored: Obj) { naive() } }

    // ---------------------------------------------------------------------------
    case class _SchemaOutputZ(c: Cls, uriString: String, urlLike: UrlLike, suffix: String) extends _SchemaOutput(c, uriString, urlLike, suffix) with AtomZO {
        def naive(ignored: Objs) { naive() } }

  // ===========================================================================
  case class _UrlLikeOutputU(ioType: IoTypeU, uriString: String, urlLike: UrlLike) extends AtomUO {
      def naive(o: Obj) { ioType.defaultFormat(o).pipe(urlLike.writeFileContent(uriString)) } }

    // ---------------------------------------------------------------------------
    case class _UrlLikeOutputZ(ioType: IoTypeZ, uriString: String, urlLike: UrlLike) extends AtomZO {
      def naive(z: Objs) { ioType.defaultFormat(z).pipe(urlLike.writeFileLines(uriString)) } }

  // ===========================================================================
  case class _OtherOutputU(ioType: IoTypeU, outlet: OutletType) extends AtomUO {
      def naive(o: Obj) { ioType.defaultFormat(o).pipe(outlet.writeLine) } }

    // ---------------------------------------------------------------------------
    case class _OtherOutputZ(ioType: IoTypeZ, outlet: OutletType) extends AtomZO {
      def naive(z: Objs) { ioType.defaultFormat(z).pipe(outlet.writeLines) } }

  // ===========================================================================
  case class _UrlLikeTableOutput(skeys: Seq[SKey], uriString : String, urlLike: UrlLike, twc: TableWritingContext) extends AtomZO {
      def naive(z: Objs) { twc.formatTable(skeys)(z).pipe(urlLike.writeFileLines(uriString)) } }

    // ---------------------------------------------------------------------------
    case class _OtherTableOutput(skeys: Seq[SKey], outlet: OutletType, twc: TableWritingContext) extends AtomZO {
      def naive(z: Objs) { twc.formatTable(skeys)(z).pipe(outlet.writeLines) } }

}

// ===========================================================================

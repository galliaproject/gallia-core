package gallia
package actions.out

import io.out._
import atoms.AtomsXO._
import actions.AtomUOs
import actions.AtomZOs

// ===========================================================================
//TODO:
// - validate _line and _content field precense for .content and .lines
// - check file/uris are valid, if file then writable
// - check extension, eg no tsv/jsonl if u (vs z)
// - if table, ensure flat

// ===========================================================================
case class ForeachOutputU(f: Obj => Unit) extends IdentityV1 with ActionUOd { def atomuo = _ForeachOutputU(f) }
case class ForeachOutputZ(f: Obj => Unit) extends IdentityV1 with ActionZOd { def atomzo = _ForeachOutputZ(f) }

// ===========================================================================
case class UrlLikeOutputU(ioType: IoTypeU, uriString: String, urlLike: UrlLike) extends ActionUOb {
    def vldt(c: Cls): Errs = Nil //TODO
    def atomuos(c: Cls): AtomUOs =
      Seq(_SchemaOutputU (c     , uriString, urlLike, DefaultSchemaSuffix), // TODO: t210115114631 - inclusion configurable
          _UrlLikeOutputU(ioType, uriString, urlLike)) }

  // ---------------------------------------------------------------------------
  case class UrlLikeOutputZ(ioType: IoTypeZ, uriString: String, urlLike: UrlLike) extends ActionZOb {
    def vldt(c: Cls): Errs = Nil //TODO
    def atomzos(c: Cls): AtomZOs =
      Seq(_SchemaOutputZ (c     , uriString, urlLike, DefaultSchemaSuffix),
          _UrlLikeOutputZ(ioType, uriString, urlLike)) }

// ===========================================================================
case class UrlLikeTableOutput(
        uriString     : String,
        urlLike       : UrlLike,
        formatConf    : FormatConf,
        nullValue     : String,
        arraySeparator: String)
      extends ActionZOb {

    def vldt(c: Cls): Errs = Nil // TODO: ensure no contradion with uriString? (eg '\t' vs tsv)
    def atomzos(c: Cls): AtomZOs =
      Seq(_SchemaOutputZ     (c      , uriString, urlLike, DefaultSchemaSuffix),
          _UrlLikeTableOutput(c.skeys, uriString, urlLike, twc))

    // ===========================================================================
    private def twc = TableWritingContext(
        fieldSeparator = formatConf.sep(uriString),
        arraySeparator = arraySeparator,
        hasHeader      = formatConf.hasHeader(uriString),
        nullValue      = nullValue)
  }

// ===========================================================================
// including JSON
case class OtherOutputU(ioType: IoTypeU, outlet: OutletType) extends ActionUOc {
    def vldt(c: Cls): Errs = Nil //TODO
    def atomuo(c: Cls): AtomUO = _OtherOutputU(ioType, outlet) }

  // ---------------------------------------------------------------------------
  // including JSON
  case class OtherOutputZ(ioType: IoTypeZ, outlet: OutletType) extends ActionZOc {
    def vldt(c: Cls): Errs = Nil //TODO
    def atomzo(c: Cls): AtomZO = _OtherOutputZ(ioType, outlet) }

  // ---------------------------------------------------------------------------
  case class OtherTableOutput(outlet: OutletType, twc: TableWritingContext) extends ActionZOc {
      def vldt(c: Cls): Errs = Nil //TODO
      def atomzo(c: Cls): AtomZO = _OtherTableOutput(c.skeys, outlet, twc) }

  // ===========================================================================
  case class RowOutput(outlet: OutletType, twc: PrettyTableWritingContext) extends ActionUOc {
        def vldt(c: Cls): Errs = Nil //TODO
        def atomuo(c: Cls): AtomUO = _RowOutput(c.skeys, outlet, twc) }
    
    // ---------------------------------------------------------------------------
    case class PrettyTableOutput(outlet: OutletType, twc: PrettyTableWritingContext) extends ActionZOc {
        def vldt(c: Cls): Errs = Nil //TODO
        def atomzo(c: Cls): AtomZO = _PrettyTableOutput(c.skeys, outlet, twc) }

  // ===========================================================================
  case class DisplayOutputU(forceRow: Boolean) extends ActionUOc {
        def vldt  (c: Cls): Errs = Nil //TODO
        def atomuo(c: Cls): AtomUO = 
          ( if (forceRow || !c.hasNesting) RowOutput(                             OutletType.StandardOutput, PrettyTableWritingContext.Default)
            else                           OtherOutputU(IoTypeU.PrettyJsonObject, OutletType.StandardOutput))
          .atomuo(c)}
    
    // ---------------------------------------------------------------------------
    case class DisplayOutputZ(forceTable: Boolean) extends ActionZOc {
        def vldt  (c: Cls): Errs = Nil //TODO
        def atomzo(c: Cls): AtomZO = 
          ( if (forceTable || !c.hasNesting) PrettyTableOutput(                    OutletType.StandardOutput, PrettyTableWritingContext.Default)
            else                             OtherOutputZ(IoTypeZ.JsonPrettyLines, OutletType.StandardOutput))
          .atomzo(c)}
  
// ===========================================================================

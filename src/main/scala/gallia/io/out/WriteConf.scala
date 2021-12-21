package gallia
package io.out

import actions.out._

// ===========================================================================
sealed trait OutputConfU { private[gallia] def actionU: ActionUO }
sealed trait OutputConfZ { private[gallia] def actionZ: ActionZO }

// ===========================================================================
case class UrlLikeNonTableConfU(ioType: IoTypeU, uriString: String, urlLike: UrlLike = UrlLike.Default) extends OutputConfU {
    def actionU = UrlLikeOutputU(ioType, uriString, urlLike) }

  // ---------------------------------------------------------------------------
  case class UrlLikeNonTableConfZ(ioType: IoTypeZ, uriString: String, urlLike: UrlLike = UrlLike.Default) extends OutputConfZ {
    def actionZ = UrlLikeOutputZ(ioType, uriString, urlLike) }

  // ===========================================================================
  case class OtherConfU(ioType: IoTypeU, outlet: OutletType) extends OutputConfU {
    def actionU = OtherOutputU(ioType, outlet) }

  // ---------------------------------------------------------------------------
  case class OtherConfZ(ioType: IoTypeZ, outlet: OutletType) extends OutputConfZ {
    def actionZ = OtherOutputZ(ioType, outlet) }

// ===========================================================================
case class OtherTableConf(outlet: OutletType, twc: TableWritingContext = TableWritingContext.Default) extends OutputConfZ {
    def actionZ = OtherTableOutput(outlet, twc)

    // ---------------------------------------------------------------------------
    def updateTableWritingContext(f: TableWritingContext => TableWritingContext) = copy(twc = f(twc))
    def sep(value: FieldSeparator) = fieldSeparator(value)
      def fieldSeparator(value: FieldSeparator) = updateTableWritingContext(_.copy(fieldSeparator = value))
      def arraySeparator(value: String)         = updateTableWritingContext(_.copy(arraySeparator = value))
      def nullValue     (value: String)         = updateTableWritingContext(_.copy(nullValue      = value))
  }

  // ---------------------------------------------------------------------------
  case class PrettyTableConf(outlet: OutletType, twc: PrettyTableWritingContext = PrettyTableWritingContext.Default) extends OutputConfZ {
    def actionZ = PrettyTableOutput(outlet, twc)

    // ---------------------------------------------------------------------------
    def updateTableWritingContext(f: PrettyTableWritingContext => PrettyTableWritingContext) = copy(twc = f(twc))
      def arraySeparator(value: String) = updateTableWritingContext(_.copy(arraySeparator = value))
      def nullValue     (value: String) = updateTableWritingContext(_.copy(nullValue      = value))
  }

  // ===========================================================================
  case class UrlLikeTableConf(
      uriString : String,
      urlLike   : UrlLike = UrlLike.Default,

      // ---------------------------------------------------------------------------
      formatConf    : FormatConf = FormatConf.Default,

      nullValue     : String     = DefaultNullValue,
      arraySeparator: String     = DefaultArraySeparator)
    extends OutputConfZ {

    def actionZ = UrlLikeTableOutput(uriString, urlLike, formatConf, nullValue, arraySeparator)

    // ---------------------------------------------------------------------------
    def updateFormatConf(f: FormatConf => FormatConf) = copy(formatConf = f(formatConf))

      def sep(sep: Char) = updateFormatConf(_.copy(explicitSeparatorOpt = Some(sep  )))

      def withHeader     = updateFormatConf(_.copy(explicitHasHeaderOpt = Some(true)))
      def noHeader       = updateFormatConf(_.copy(explicitHasHeaderOpt = Some(false)))
  }

// ===========================================================================

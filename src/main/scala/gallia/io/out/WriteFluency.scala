package gallia.io
package out

// ===========================================================================
class UrlLikeNonTableFluencyU(val conf: UrlLikeNonTableConfU)
        extends FluencyBase[UrlLikeNonTableFluencyU, UrlLikeNonTableConfU](new UrlLikeNonTableFluencyU(_))

        with    HasCharsetFluency    [UrlLikeNonTableFluencyU]
        with    HasCompressionFluency[UrlLikeNonTableFluencyU]

        with EndWriteUFluency {
      val self = this

      // ---------------------------------------------------------------------------
      // boilerplate:
      override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
      override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))
    }

// ===========================================================================
class UrlLikeNonTableFluencyZ(val conf: UrlLikeNonTableConfZ)
        extends FluencyBase[UrlLikeNonTableFluencyZ, UrlLikeNonTableConfZ](new UrlLikeNonTableFluencyZ(_))

        with    HasCharsetFluency    [UrlLikeNonTableFluencyZ]
        with    HasCompressionFluency[UrlLikeNonTableFluencyZ]

        with EndWriteZFluency {
      val self = this

      // ---------------------------------------------------------------------------
      // boilerplate:
      override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
      override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))
    }

// ===========================================================================
class UrlLikeTableFluency(val conf: UrlLikeTableConf)
      extends FluencyBase[UrlLikeTableFluency, UrlLikeTableConf](new UrlLikeTableFluency(_))

      with    HasCharsetFluency    [UrlLikeTableFluency]
      with    HasCompressionFluency[UrlLikeTableFluency]

      with EndWriteZFluency {
    val self = this

    // ---------------------------------------------------------------------------
    def noHeader  : UrlLikeTableFluency = conf.updateFormatConf(_.copy(explicitHasHeaderOpt = Some(false)))
    def withHeader: UrlLikeTableFluency = conf.updateFormatConf(_.copy(explicitHasHeaderOpt = Some(true )))

    // ---------------------------------------------------------------------------
    // boilerplate:
    override def charset    (value: SupportedCharset)     = conf.copy(urlLike = conf.urlLike.update(value))
    override def compression(value: SupportedCompression) = conf.copy(urlLike = conf.urlLike.update(value))
  }

// ===========================================================================
class OtherFluencyU(outlet: OutletType) extends EndWriteUFluency {
    val conf = json.conf

    // ===========================================================================
    def content = endOtherU(IoTypeU.RawContent,        outlet)
    
    def json          = prettyJson    
      def compactJson = endOtherU(IoTypeU.CompactJsonObject, outlet)
      def prettyJson  = endOtherU(IoTypeU.PrettyJsonObject,  outlet)
  }

  // ===========================================================================
  class OtherFluencyZ(outlet: OutletType) extends EndWriteZFluency {
    val conf = jsonLines.conf

    // ===========================================================================
    def lines = endOtherZ(IoTypeZ.RawLines, outlet)

    // ---------------------------------------------------------------------------
    // TODO: t210104111151 - offer to write JSON array as one-liner (though we want to discourage its use)?
    def jsonl = jsonLines

      def jsonLines = endOtherZ(IoTypeZ.JsonLines, outlet)
      def jsonArray = endOtherZ(IoTypeZ.JsonArray, outlet)

    // ---------------------------------------------------------------------------
    def table(sep: Char): OtherTableFluency = new OtherTableFluency(OtherTableConf(outlet).sep(sep))
      def tsv: OtherTableFluency = table('\t')
      def csv: OtherTableFluency = table(',')
  }

// ===========================================================================
class OtherTableFluency(val conf: OtherTableConf)
      extends FluencyBase[OtherTableFluency, OtherTableConf](new OtherTableFluency(_))
      with    EndWriteZFluency {
    val self = this

    // ---------------------------------------------------------------------------
    def updateTableWritingContext(f: TableWritingContext => TableWritingContext): OtherTableFluency = conf.copy(twc = f(conf.twc))

    def arraySeparator(value: String)         = updateTableWritingContext(_.copy(arraySeparator = value))
    def nullValue     (value: String)         = updateTableWritingContext(_.copy(nullValue      = value))
}

// ===========================================================================

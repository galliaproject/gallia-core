package gallia
package io

// ===========================================================================
case class FormatConf(
    explicitSeparatorOpt: Option[FieldSeparator],
    explicitHasHeaderOpt: Option[Boolean]) {
  import FormatConf._

  // ---------------------------------------------------------------------------
  def sep      (inputString: String): Char    = TableSepStatus   (extensionPair(inputString)._1, explicitSeparatorOpt).resolveSep
  def hasHeader(inputString: String): Boolean = TableHeaderStatus(extensionPair(inputString)._2, explicitHasHeaderOpt).resolveHasHeader
}

// ===========================================================================
object FormatConf {
  val Default = FormatConf(None, None)

  // ---------------------------------------------------------------------------
  private case class TableHeaderStatus(
        extensionOpt: Option[Boolean],
        explicitOpt : Option[Boolean]) {

      def resolveHasHeader: Boolean =
        (extensionOpt, explicitOpt) match {
          case (None          , None          ) => true // assumes present if no indication
          case (Some(inferred), None          ) => inferred
          case (_             , Some(explicit)) => explicit }

    }

    // ---------------------------------------------------------------------------
    private case class TableSepStatus(
        extensionOpt: Option[Char],
        explicitOpt : Option[Char]) {

      def resolveSep: Char =
        (extensionOpt, explicitOpt) match {
          case (None          , None          ) => '\t' // assumes tab if no indication
          case (Some(inferred), None          ) => inferred
          case (_             , Some(explicit)) => explicit }

    }

  // ===========================================================================
  private def extensionPair(inputString: String): (Option[Char], Option[Boolean]) =
    SupportedExtensions
        .parseOpt(inputString)
        .map {
          case SupportedExtensions.tsv  => (Some('\t'), None)
          case SupportedExtensions.csv  => (Some( ','), None)

          case SupportedExtensions.tsvwh => (Some('\t'), Some(true ))
          case SupportedExtensions.tsvnh => (Some('\t'), Some(false))

          case SupportedExtensions.csvwh => (Some(','), Some(true ))
          case SupportedExtensions.csvnh => (Some(','), Some(false))

          case _ => (None, None) }
        .getOrElse((None, None))

}

// ===========================================================================

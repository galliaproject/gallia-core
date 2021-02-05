package gallia.io

import gallia.io.out.OutletType

// ===========================================================================
case class UrlLike(
      explicitCharset    : Option[SupportedCharset],
      explicitCompression: Option[SupportedCompression]) {

    def update(value: SupportedCharset    ): UrlLike = copy(explicitCharset     = Some(value))
    def update(value: SupportedCompression): UrlLike = copy(explicitCompression = Some(value))

    // ---------------------------------------------------------------------------
    def resolveCharset                          : SupportedCharset     = explicitCharset    .getOrElse(SupportedCharset.Default) // TODO: t201216102258 - option to detect?
    def resolveCompression(urlString: UrlString): SupportedCompression = explicitCompression.getOrElse(SupportedCompression.parse(urlString))

    // ---------------------------------------------------------------------------
    def flwc(urlString: String): out.FileLikeWriteContext =
      out.FileLikeWriteContext(
        urlString,
        charset     = resolveCharset,
        compression = resolveCompression(urlString))

    // ---------------------------------------------------------------------------
    final def writeFileContent(urlString: String) = OutletType.file1(flwc(urlString)) _
    final def writeFileLines  (urlString: String) = OutletType.file2(flwc(urlString)) _
  }

  // ===========================================================================
  object UrlLike {

    val Default = UrlLike(
      explicitCharset     = None,
      explicitCompression = None)

  }

// ===========================================================================
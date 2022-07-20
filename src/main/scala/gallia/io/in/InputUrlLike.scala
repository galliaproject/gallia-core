package gallia
package io.in

import java.net.URL
import aptus._
import aptus.aptutils.InputStreamUtils
import data.multiple.streamer.{IteratorStreamer, Streamer, ViewStreamer}

// ===========================================================================
case class InputUrlLike( // TODO: t210115193904 - check URI, regular file vs symlink, ...
    private val inputString : InputString,

    charset     : SupportedCharset     = SupportedCharset    .UTF_8,
    compression : SupportedCompression = SupportedCompression.NoCompression,

    linesPreprocessing: Option[LinesPreprocessing] = None) {

  val _inputString = InputUrlLike.normalize(inputString) // TODO: validation

  // ===========================================================================
  def content(): Content = _inputString.pipe(new URL(_)).pipe(_content())

  // ---------------------------------------------------------------------------
  @Distributivity
  def firstLine(): Line = // TODO: t210114154924 - generalize to n lines + make optional + cache
    IteratorStreamer
      .from4(
        new data.DataRegenerationClosure[Line] {
          def regenerate = () => lines() })
      .pipeOpt(linesPreprocessing)(f => (x: Streamer[Line]) => f(x).asIteratorBased.asInstanceOfIteratorStreamer)
      .firstOpt() // will close
      .get // TODO: see t210114154924

  // ---------------------------------------------------------------------------
  def streamLines(inMemoryMode: Boolean): Streamer[Line] =
    InputUrlLike
      .hackOpt
      .map(_.apply(this))
      .getOrElse {
        if (inMemoryMode)
          ViewStreamer.from(lines().consumeAll)
        else
          IteratorStreamer.from4(
            new data.DataRegenerationClosure[Line] {
              def regenerate = () => lines() }) }
      .pipeOpt(linesPreprocessing)(f => _.pipe(f))

  // ===========================================================================
  private def _content()(url: URL): Content =
    compression match {

      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.content(url, charset.charset)

      case SupportedCompression.Zip =>
        url.toExternalForm().stripPrefix("file:") /* FIXME? */.pipe(aptus.aptmisc.Zip.content) }

  // ===========================================================================
  private def lines(): CloseabledIterator[Line] =
    compression match {
      case SupportedCompression.NoCompression | SupportedCompression.Gzip | SupportedCompression.Bzip2 =>
        InputStreamUtils.lines(
          new URL(_inputString),
          charset.charset)

      case SupportedCompression.Zip => ??? /* TODO */ }
}

// ===========================================================================
object InputUrlLike {
  var hackOpt: Option[InputUrlLike => Streamer[Line]] = None // until dust settles and we do proper injection

  // ---------------------------------------------------------------------------
  private def normalize(value: String): String = SupportedUriScheme.file.normalizeOpt(value).getOrElse(value)
  
  // ---------------------------------------------------------------------------
  def streamer[T](inMemoryMode: Boolean)(iter: CloseabledIterator[T]): Streamer[T] = // TODO: move + include hack above properly?
    if (inMemoryMode)     ViewStreamer.from(iter.consumeAll)
    else              IteratorStreamer.from(iter)
}

// ===========================================================================
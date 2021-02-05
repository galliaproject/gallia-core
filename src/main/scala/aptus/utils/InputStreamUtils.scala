package aptus.utils

import java.io._

import scala.collection.JavaConverters.asScalaIteratorConverter

import org.apache.commons.compress.compressors.CompressorStreamFactory

import aptus.InputStream_
import aptus.URL_
import aptus.{Content, Line}

// ===========================================================================
object InputStreamUtils {

  def content(url: URL                    , charset: Charset): Content = content(url.smartCloseabledInputStream, charset)
  def content(cis: Closeabled[InputStream], charset: Charset): Content = cis.consume(toStr(_, charset))

  // ---------------------------------------------------------------------------
  def lines(url: URL                    , charset: Charset): Closeabled[Iterator[Line]] = lines(url.smartCloseabledInputStream, charset)
  def lines(cis: Closeabled[InputStream], charset: Charset): Closeabled[Iterator[Line]] =
    cis // TODO: too cryptic, take apart
      .flatMap {
        _ .closeabledBufferedReader(charset)
          .map(_.lines().iterator.asScala) }

  // ===========================================================================
  def toStr(is: InputStream, charset: Charset): String = {
    val scanner = // per https://stackoverflow.com/questions/309424/how-do-i-read-convert-an-inputstream-into-a-string-in-java
      new java.util.Scanner(is, charset.name)
        .useDelimiter("\\A") // \A The beginning of the input

    val content: String = if (scanner.hasNext()) scanner.next() else ""

    scanner.close()
    is.close()

    content
  }

  // ===========================================================================
  def closeabledBufferedReader(is: InputStream, charset: Charset): Closeabled[BufferedReader] = {
    val isr = new InputStreamReader(is, charset)
    val br  = new BufferedReader(isr)

    Closeabled.from(br, Seq(br, isr))
  }

  // ---------------------------------------------------------------------------
  def smartCloseabledInputStream(fis: InputStream): Closeabled[InputStream] = {
    val bis    = new java.io.BufferedInputStream(fis)
    val is     = smartInputStream(bis)

    aptus.Closeabled.from(is, Seq(is, bis, fis))
  }

  // ---------------------------------------------------------------------------
  def smartInputStream(bis: BufferedInputStream): InputStream =
    util.Try(CompressorStreamFactory.detect(bis)) // TODO: efficiency...
      .toOption match {
        case None    =>                                                           bis
        case Some(_) => new CompressorStreamFactory().createCompressorInputStream(bis) }

}

// ===========================================================================

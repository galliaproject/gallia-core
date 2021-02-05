package gallia.io

// ===========================================================================
trait HasSelf[T] { protected[io] val self: T }

  // ---------------------------------------------------------------------------
  abstract class FluencyBase[Fluency, Conf](val construct: Conf => Fluency) extends HasSelf[Fluency] {
    protected implicit def _to(updateConf: Conf): Fluency = construct(updateConf) }

// ===========================================================================
trait HasCharsetFluency[Fluency] {
  def charset    (value: SupportedCharset)     : Fluency
  def charset    (f: SupportedCharset.Selector): Fluency = charset(f(SupportedCharset))

    // ---------------------------------------------------------------------------
    def utf8  = charset(_.UTF_8)
    def iso   = charset(_.ISO_8859_1)
    def ascii = charset(_.US_ASCII)
}

// ===========================================================================
trait HasCompressionFluency[Fluency] {
  def compression(value: SupportedCompression)     : Fluency
  def compression(f: SupportedCompression.Selector): Fluency = compression(f(SupportedCompression))

    // ---------------------------------------------------------------------------
    def gzip  = compression(_.Gzip)
    def bzip2 = compression(_.Bzip2)
    def zip   = compression(_.Zip)
}

// ===========================================================================

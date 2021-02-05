package gallia.io

// ===========================================================================
package object out extends gallia._io with gallia.__io {
  trait EndWriteUFluency { protected[gallia] val conf: OutputConfU }
  trait EndWriteZFluency { protected[gallia] val conf: OutputConfZ }

  // ===========================================================================
  val StartU: StartWriteUFluency = new StartWriteUFluency()
  val StartZ: StartWriteZFluency = new StartWriteZFluency()

  // ---------------------------------------------------------------------------
  def endOtherU(ioType: IoTypeU, outlet: OutletType): EndWriteUFluency = new EndWriteUFluency { val conf = OtherConfU(ioType, outlet) }
  def endOtherZ(ioType: IoTypeZ, outlet: OutletType): EndWriteZFluency = new EndWriteZFluency { val conf = OtherConfZ(ioType, outlet) }

  // ===========================================================================
  type StringWriter = java.io.StringWriter

  // ===========================================================================
  case class FileLikeWriteContext(
        urlString: String,

        charset    : SupportedCharset,
        compression: SupportedCompression,

        flushModuloOpt: Option[Long]         = None,
        logModuloOpt  : Option[Long => Unit] = None) {

      def outputStreamWriter(): java.io.OutputStreamWriter = compression.outputStreamWriter(urlString)

      def flushModulo = flushModuloOpt.getOrElse(FileLikeWriteContext.DefaultFlushModulo)

      def log(counter: Long) { logModuloOpt.foreach(_.apply(counter)) }
    }

    // ===========================================================================
    object FileLikeWriteContext {
      private lazy val CompressorStreamFactory = new org.apache.commons.compress.compressors.CompressorStreamFactory()

      val DefaultFlushModulo = 100L
    }
}

// ===========================================================================
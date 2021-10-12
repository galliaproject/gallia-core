package gallia
package io.out

// ===========================================================================
class StartWriteUFluency() extends EndWriteUFluency {
    val conf: OutputConfU = stdout.json.conf

    // ---------------------------------------------------------------------------
    private[gallia] def entirelyUriDriven(path: String) = new EntirelyUriDrivenFluencyU(OutputStringDrivenConf.confU(path))

    // ===========================================================================
    private[gallia] def formatString(sw: StringWriter): OtherFluencyU = new OtherFluencyU(OutletType.FormattedString(sw))

      def stdout: OtherFluencyU = new OtherFluencyU(OutletType.StandardOutput)
      def stderr: OtherFluencyU = new OtherFluencyU(OutletType.StandardError)

    // ===========================================================================
    def file(path: String) = url(path)

    def url(value: String)      : UrlFluencyU = new UrlFluencyU(value)
    def url(value: java.net.URL): UrlFluencyU = url(value.toExternalForm)

    def uri(value: String)      : UriFluencyU = new UriFluencyU(value)
    def uri(value: java.net.URI): UriFluencyU = uri(value.toASCIIString)
  }

  // ===========================================================================
  class StartWriteZFluency() extends EndWriteZFluency {
    val conf: OutputConfZ = stdout.jsonl.conf

    // ---------------------------------------------------------------------------
    private[gallia] def entirelyUriDriven(path: String) = new EntirelyUriDrivenFluencyZ(OutputStringDrivenConf.confZ(path))

    // ===========================================================================
    private[gallia] def formatString(sw: StringWriter): OtherFluencyZ = new OtherFluencyZ(OutletType.FormattedString(sw))

      def stdout: OtherFluencyZ = new OtherFluencyZ(OutletType.StandardOutput)
      def stderr: OtherFluencyZ = new OtherFluencyZ(OutletType.StandardError)

    // ===========================================================================
    def file(path: String) = url(path)

    def url(value: String)      : UrlFluencyZ = new UrlFluencyZ(value)
    def url(value: java.net.URL): UrlFluencyZ = url(value.toExternalForm)

    def uri(value: String)      : UriFluencyZ = new UriFluencyZ(value)
    def uri(value: java.net.URI): UriFluencyZ = uri(value.toASCIIString)

    // ===========================================================================
    @deprecated def tsv(s: String) = file(s).tsv // TODO: or keep those shorthands?
  }

// ===========================================================================

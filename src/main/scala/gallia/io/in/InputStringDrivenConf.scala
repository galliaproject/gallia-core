package gallia.io.in

import aptus.Anything_
import aptus.UriString
import gallia._

// ===========================================================================
trait InputStringDrivenConfHelper[InputAction, IoType] {
    val inputString: String

    // ---------------------------------------------------------------------------
    val parsePathOpt: UriString => Option[IoType]

    // ---------------------------------------------------------------------------
    protected def jsonStringAction           : InputAction
    protected def urlLikeAction(tipe: IoType): InputAction
    protected def otherAction(scheme: SupportedUriScheme): InputAction

    // ===========================================================================
    def _action: InputAction =
      InputStringType
        .parse(inputString)
         match {
          // TODO: forbid Projection + check not querying
          case InputStringType.JsonObject  => jsonStringAction
          case InputStringType.JsonArray   => jsonStringAction
          case InputStringType.Indirection => indirection }

      // ===========================================================================
      protected def indirection: InputAction =
        util.Try(new java.net.URI(inputString)) match {
          case util.Failure(f) => ???// TODO
          case util.Success(uri) =>
            Option(uri.getScheme)
              .thn(SupportedUriScheme.parse)
               match {
                case SupportedUriScheme.file | SupportedUriScheme.http | SupportedUriScheme.https | SupportedUriScheme.ftp | SupportedUriScheme.sftp =>
                  Option(uri.getPath) match {
                    case None          => ???
                    case Some(uriPath) =>
                      (parsePathOpt(uriPath) match {
                          case None       => ???
                          case Some(tipe) => urlLikeAction(tipe) }) }
                case scheme => otherAction(scheme) } }

  }

  // ===========================================================================
  trait InputUStringDrivenConfHelper extends InputStringDrivenConfHelper[ActionIU, IoTypeU] { self: InputUStringDrivenConf =>
    val parsePathOpt = IoTypeU.parsePathOpt _

    private def start = new StartReadUFluency(inputString)

    // ===========================================================================
    protected def jsonStringAction = IoTypeU.DirectJsonObject.defaultRead(start, self).conf.actionU

    // ---------------------------------------------------------------------------
    protected def urlLikeAction(tipe: IoTypeU) = tipe.defaultRead(start, self).conf.actionU

    // ===========================================================================
    def otherAction(scheme: SupportedUriScheme) =
      scheme match {
        case SupportedUriScheme.s3      => ???
        case SupportedUriScheme.hdfs    => ???

        // ---------------------------------------------------------------------------
        case SupportedUriScheme.jdbc    => ??? // TODO: illegal unsupported (for now)
        case SupportedUriScheme.mongodb => ??? // TODO: illegal unsupported (for now)

        // ---------------------------------------------------------------------------
        case x => x.p__; ???
     }
  }

  // ===========================================================================
  trait InputZStringDrivenConfHelper extends InputStringDrivenConfHelper[ActionIZ, IoTypeZ] { self: InputZStringDrivenConf =>
    val parsePathOpt = IoTypeZ.parsePathOpt _

    private def start = new StartReadZFluency(inputString)

    // ===========================================================================
    protected def jsonStringAction  = IoTypeZ.DirectJsonArray.defaultRead(start, self).conf.actionZ

    // ---------------------------------------------------------------------------
    protected def urlLikeAction(tipe: IoTypeZ) = tipe.defaultRead(start, self).conf.actionZ

    // ===========================================================================
    def otherAction(scheme: SupportedUriScheme) =
      scheme match {
        case SupportedUriScheme.s3      => ???
        case SupportedUriScheme.hdfs    => ???

        // ===========================================================================
        case SupportedUriScheme.jdbc    => start. jdbc   .querying(queryingOpt).conf.actionZ
        case SupportedUriScheme.mongodb => start._mongodb.querying(queryingOpt).conf.actionZ

        // ===========================================================================
        case x => x.p__; ???
      }
  }

// ===========================================================================

package gallia.io.out

import aptus.Anything_

// ===========================================================================
object OutputStringDrivenConf {

  def confU(path: String): OutputConfU =
    util.Try(new java.net.URI(path)) match {
      case util.Failure(f) => ???// TODO
      case util.Success(uri) =>
        Option(uri.getScheme)
          .thn(SupportedUriScheme.parse)
           match {
            case SupportedUriScheme.file | SupportedUriScheme.http | SupportedUriScheme.https | SupportedUriScheme.ftp | SupportedUriScheme.sftp =>
              Option(uri.getPath) match {
                case None       => ???
                case Some(path) => urlLikeConfU(path) }

            // ===========================================================================
            case SupportedUriScheme.s3      => ???//HeadZ.Dummy //TODO
            case SupportedUriScheme.hdfs    => ???//HeadZ.Dummy //TODO

            // ---------------------------------------------------------------------------
            case SupportedUriScheme.jdbc    => ???//HeadsIn.inputZ(jdbc)
            case SupportedUriScheme.mongodb => ???//HeadsIn.inputZ(mongodb)

            // ===========================================================================
            case x => x.p__; ???
          }
    }

  // ===========================================================================
  def confZ(path: String): OutputConfZ =
    util.Try(new java.net.URI(path)) match {
      case util.Failure(f) => ???// TODO
      case util.Success(uri) =>
        Option(uri.getScheme)
          .thn(SupportedUriScheme.parse)
           match {
            case SupportedUriScheme.file | SupportedUriScheme.http | SupportedUriScheme.https | SupportedUriScheme.ftp | SupportedUriScheme.sftp =>
              Option(uri.getPath) match {
                case None       => ???
                case Some(path) => urlLikeConfZ(path) }

            // ===========================================================================
            case SupportedUriScheme.s3      => ???//HeadZ.Dummy //TODO
            case SupportedUriScheme.hdfs    => ???//HeadZ.Dummy //TODO

            // ---------------------------------------------------------------------------
            case SupportedUriScheme.jdbc    => ???//HeadsIn.inputZ(jdbc)
            case SupportedUriScheme.mongodb => ???//HeadsIn.inputZ(mongodb)

            // ===========================================================================
            case x => x.p__; ???
          }
    }

  // ===========================================================================
  def urlLikeConfU(path: String): OutputConfU =
       IoTypeU
         .parsePathOpt(path)
         .getOrElse(IoTypeU.Default)
         .urlLikeConf(path)

    // ---------------------------------------------------------------------------
    def urlLikeConfZ(path: String): OutputConfZ =
       IoTypeZ
         .parsePathOpt(path)
         .getOrElse(IoTypeZ.Default) //TODO: gz by default?
         .outputConf(path)

}

// ===========================================================================

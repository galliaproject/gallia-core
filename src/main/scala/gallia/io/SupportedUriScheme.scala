package gallia
package io

import enumeratum.{Enum, EnumEntry}
import aptus.{String_, Option_, Unit_}

// ===========================================================================
sealed trait SupportedUriScheme extends EnumEntry

  // ---------------------------------------------------------------------------
  sealed trait UrlLikeScheme extends SupportedUriScheme

  // ===========================================================================
  object SupportedUriScheme extends Enum[SupportedUriScheme] { val values = findValues
    val Default = file

    // ---------------------------------------------------------------------------
    def parse(scheme: Option[String]): SupportedUriScheme =
      scheme
        .map(SupportedUriScheme.withNameOption)
        .map(_.force) //TODO:wrap: unknown scheme, must provide custom
        .getOrElse(SupportedUriScheme.Default)

    // ===========================================================================
    case object  file extends UrlLikeScheme {
      lazy val Home = ().fs.homeDirectoryPath()

      // ---------------------------------------------------------------------------
      def normalizeOpt(value: String): Option[String] =
             if (value.startsWith( "/")) Some(value                   .prepend("file://"))
        else if (value.startsWith("~/")) Some(value.tail.prepend(Home).prepend("file://"))
        else                             None
    }

    // ---------------------------------------------------------------------------
    case object  http     extends UrlLikeScheme
    case object  https    extends UrlLikeScheme

    case object  ftp      extends UrlLikeScheme
    case object sftp      extends UrlLikeScheme
    //TODO: scp?

    case object  jdbc     extends SupportedUriScheme
    case object  mongodb  extends SupportedUriScheme

    case object  s3       extends SupportedUriScheme
    case object  hdfs     extends SupportedUriScheme // eg "hdfs://namenodehost1/file1"

    // ---------------------------------------------------------------------------
    // TODO:
    // - mongodb+srv
    // - neo4j+s, bolt+routing?
    // - alluxio://, gluster://, ipfs://, ...

  }

// ===========================================================================

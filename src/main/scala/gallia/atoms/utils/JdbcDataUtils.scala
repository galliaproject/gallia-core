package gallia
package atoms
package utils

import aptus._
import aptus.ResultSet_
import data.multiple.streamer.IteratorStreamer

// ===========================================================================
private[atoms] object JdbcDataUtils {

  def jdbcData(uri: java.net.URI)(schemaOpt: Option[Cls])(sqlQuery: String) =//: Objs =
      aptus.aptmisc
        .Rdbms(uri)
        .query2(sqlQuery)
        .pipe(JdbcDataUtils.objs(schemaOpt))

    // ---------------------------------------------------------------------------
    def jdbcData(connection: java.sql.Connection)(schemaOpt: Option[Cls])(sqlQuery: String) = //: Objs =
      aptus.aptmisc
        .Rdbms (connection)
        .query2(sqlQuery)
        .pipe(JdbcDataUtils.objs(schemaOpt))

    // ---------------------------------------------------------------------------
    private def objs(schemaOpt: Option[Cls])(rs: aptus.Closeabled[java.sql.ResultSet]): CloseabledIterator[Obj] = //: Objs =
      rs
        .map(_.rawRdbmsEntries)
        .toCloseabledIterator
        .map { entries => obj(entries.toList /* from Map */) }
        .map { o => schemaOpt match {
          case None         =>                                                  o
          case Some(schema) => data.JdbcToGalliaData.convertRecursively(schema)(o) } }

  // ===========================================================================
  def extractTableNameOpt(inputString: String, param: String): Option[String] = // not standard...
    inputString // FIXME: t210115205609 - decoding
      .splitBy("&")
      .find(_.startsWith (s"${param}="))
      .map (_.stripPrefix(s"${param}="))

}

// ===========================================================================

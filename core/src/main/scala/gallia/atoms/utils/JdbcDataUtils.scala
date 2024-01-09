package gallia
package atoms
package utils

import aptus._
import aptus.ResultSet_

// ===========================================================================
private[atoms] object JdbcDataUtils {

  def jdbcData(uri: java.net.URI)(schemaOpt: Option[Cls])(sqlQuery: String): CloseabledIterator[Obj] =
      aptus.aptmisc
        .Rdbms(uri)
        .query2(sqlQuery)
        .pipe(objs(schemaOpt))

    // ---------------------------------------------------------------------------
    def jdbcData(connection: java.sql.Connection)(schemaOpt: Option[Cls])(sqlQuery: String): CloseabledIterator[Obj] =
      aptus.aptmisc
        .Rdbms (connection)
        .query2(sqlQuery)
        .pipe(objs(schemaOpt))

    // ---------------------------------------------------------------------------
    private def objs(schemaOpt: Option[Cls])(rs: Closeabled[java.sql.ResultSet]): CloseabledIterator[Obj] = {
      val tmp: Closeabled[Iterator[RawRdbmsEntries]] = rs.map(_.rawRdbmsEntries)

      new CloseabledIterator(tmp.underlying, tmp.cls) // can't use .toCloseabledIterator, causes issues with scala 2.12: "Cannot prove that Iterator[aptus.RawRdbmsEntries] =:= Iterator[U]"
        .map { entries => obj(entries.toList /* from Map */) }
        .map { o => schemaOpt match {
          case None         =>                                                  o
          case Some(schema) => data.JdbcToGalliaData.convertRecursively(schema)(o) } } }

  // ===========================================================================
  def extractTableNameOpt(inputString: String, param: String): Option[String] = // not standard...
    inputString // FIXME: t210115205609 - decoding
      .splitBy("&")
      .find(_.startsWith (s"${param}="))
      .map (_.stripPrefix(s"${param}="))

}

// ===========================================================================

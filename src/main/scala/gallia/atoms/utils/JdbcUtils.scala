package gallia
package atoms
package utils

import aptus._
import aptus.ResultSet_
import data.multiple.streamer.Streamer

// ===========================================================================
private[atoms] object JdbcUtils {

  def objsOpt(schemaOpt: Option[Cls])(rs: aptus.Closeabled[java.sql.ResultSet]): Option[Objs] =
    Streamer.fromIterator(
        rs.map(_.rawRdbmsEntries))
      .map(_.toList)
      .map(obj)
      .map { o: Obj =>
        schemaOpt
          .map(data.JdbcTax.payUp(_)(o))
          .getOrElse(o) }
      .pipe(Objs.build(_))
      .in.some

  // ===========================================================================
  def extractTableNameOpt(inputString: String, param: String): Option[String] = // not standard...
    inputString // FIXME: t210115205609 - decoding
      .splitBy("&")
      .find(_.startsWith (s"${param}="))
      .map (_.stripPrefix(s"${param}="))

}

// ===========================================================================

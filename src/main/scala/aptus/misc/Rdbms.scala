package aptus.misc

import java.sql.{Array => _, _}

import aptus.{Anything_, String_, Seq_}
import aptus.ResultSet_
import aptus.{QueryString, TableName, UriString, RawRdbmsEntries}

// ===========================================================================
object Rdbms {
  def apply(uri: UriString)        = new UriQuerier(new URI(uri))
  def apply(uri: URI)              = new UriQuerier(uri)
  def apply(conn: Connection)      = new ConnectionQuerier(conn)

  // ---------------------------------------------------------------------------
  def apply(ps: PreparedStatement) = new PreparedStatementQuerier(ps)

  // ===========================================================================
  implicit class SqlConnection__(conn: java.sql.Connection) {
    def closeable = new Closeable { def close() = conn.close() }

    def querier = new ConnectionQuerier(conn)

    def querier(query: QueryString) = conn.prepareStatement(query).thn(new PreparedStatementQuerier(_))
  }

  // ---------------------------------------------------------------------------
  implicit class PreparedStatement_(ps: PreparedStatement) {
    def parameterCount: Int = ps.getParameterMetaData.getParameterCount

    def set(index: Int, value: Any): PreparedStatement = { ps.setObject(index, value); ps }

    def setAll(value1: Any, more: Any*): PreparedStatement = {
      val values = value1 +: more
      require(values.size == parameterCount, (values.size, parameterCount, values.#@@))

      ps.clearParameters()
      values.zipWithRank.foldLeft(ps)((tmp, entry) => tmp.set(entry._2, entry._1))
    }
  }

  // ===========================================================================
  final class ConnectionQuerier(conn : Connection) extends BasicQuerier with Closeable {
    def close() { conn.close() }

    /** beware: unsanitized! - TODO: t210114145431 */
    def query(query: QueryString): (ResultSet, Closeable) = {
      val ps   : PreparedStatement = conn.prepareStatement(query)
      val rs   : ResultSet         = ps.executeQuery()

      (rs,
       new Closeable {
          override def close() {
            rs.close()
            ps.close() } } )
    }

  }

  // ===========================================================================
  final class UriQuerier(uri: URI) extends BasicQuerier {

    def selectAll(table: TableName): (ResultSet, Closeable) =
      table
        .require(_.nonEmpty)
        .require(_.forall(aptus.utils.CharUtils.AlphaNumericalWithUnderscoreSet.contains)) /* cheap sanitizing.. */
        .thn(name => query(s"SELECT * FROM ${name}"))

    // ---------------------------------------------------------------------------
    /** beware: unsanitized! TODO: t210114145431 */
    def query(query: QueryString): (ResultSet, Closeable) = {
      val conn : Connection        = DriverManager.getConnection(uri.toString)
      val ps   : PreparedStatement = conn.prepareStatement(query)
      val rs   : ResultSet         = ps.executeQuery()

      (rs,
       new Closeable {
          override def close() {
            rs  .close()
            ps  .close()
            conn.close() } } )
    }
  }

  // ===========================================================================
  final class PreparedStatementQuerier(ps: PreparedStatement) extends AdvancedQuerier with Closeable {
    def close() { ps.close() }

    /** beware: unsanitized! */
    def query(f: PreparedStatement => PreparedStatement): (ResultSet, Closeable) = f(ps).executeQuery().thn { rs => (rs, rs.closeable ) }
  }

  // ===========================================================================
  trait BasicQuerier {
    def query(query: QueryString): (ResultSet, Closeable)
  }

  // ---------------------------------------------------------------------------
  trait AdvancedQuerier {
    def query(f: PreparedStatement => PreparedStatement): (ResultSet, Closeable)
    def query                                           : (ResultSet, Closeable) = query(identity)
  }

  // ===========================================================================
  def generalize(row: RawRdbmsEntries): List[(Symbol, Option[Any])] =
    row
      .map { case (key, value) =>
        key.symbol -> value.map(Java.toScala) }
      .toList

}

// ===========================================================================

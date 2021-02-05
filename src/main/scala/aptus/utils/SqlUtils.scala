package aptus.utils

import java.sql._

import aptus.Seq_
import aptus.{RawRdbmsEntries, RawRdbmsValues}

// ===========================================================================
object SqlUtils {

  def columnNames(rs: ResultSet): Vector[String] = {
    val metaData: ResultSetMetaData = rs.getMetaData

    Range(1, metaData.getColumnCount + 1)
      .map(metaData.getColumnName)
      .toVector
  }

  // ===========================================================================
  def rawRdbmsEntries(rs: ResultSet): Iterator[RawRdbmsEntries] = {
    val columnNames = this.columnNames(rs) // for caching

    rawRdbmsValues(rs)
      .map { row =>
        columnNames
          .zip(row)
          .force.map }
  }

  // ---------------------------------------------------------------------------
  def rawRdbmsValues(rs: ResultSet): Iterator[RawRdbmsValues] = {
    val columnNames = this.columnNames(rs) // for caching

    new Iterator[RawRdbmsValues] {

      // https://stackoverflow.com/questions/1870022/java-iterator-backed-by-a-resultset
      var _didNext = false
      var _hasNext = false

      // ---------------------------------------------------------------------------
      def hasNext = {
        if (!_didNext) {
          _hasNext = rs.next()
          _didNext = true
        }

        _hasNext
      }

      // ---------------------------------------------------------------------------
      def next() = {
        if (!_didNext) { rs.next() }
        _didNext = false

        columnNames
          .map(rs.getObject)
          .map(Option.apply /* because could be (java) null */)
      }
    }
  }

}

// ===========================================================================

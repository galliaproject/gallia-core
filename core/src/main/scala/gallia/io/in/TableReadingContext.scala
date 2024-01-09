package gallia
package io
package in

import org.apache.commons.csv.CSVFormat

// ===========================================================================
trait TableIoContext {
        val fieldSeparator: FieldSeparator
        val arraySeparator: String
        val hasHeader     : Boolean
        val nullValue     : String

    // ---------------------------------------------------------------------------
    protected lazy val Format =
      TableIoContext
        .DefaultTableFormat
        .withDelimiter(fieldSeparator) // TODO: cache common ones
        .withRecordSeparator('\n')
        .withAllowDuplicateHeaderNames()

  }

  // ---------------------------------------------------------------------------
  object TableIoContext {
    private lazy val DefaultTableFormat = CSVFormat.DEFAULT
  }

// ===========================================================================

package gallia
package meta

import aptus.{String_, Seq_}
import aptus.DebugString

// ===========================================================================
object MetaObj0 {

  def formatClassDebug(value: Cls): DebugString =
    value
      .fields
      .map(formatFieldDebug)
      .joinln

    // ---------------------------------------------------------------------------
    private def formatFieldDebug(value: Fld): DebugString =
        value
          .key
          .name
          .padRight(16, ' ')
          .tab(value.info.pipe(formatInfoDebug))

      // ---------------------------------------------------------------------------
      private def formatInfoDebug(value: Info): DebugString = {
          if(value.isRequired)                              value.union.map(formatSubInfoDebug).join("|")
          else                 s"${formatOptional(true)}\t${value.union.map(formatSubInfoDebug).join("|")}" }

        // ---------------------------------------------------------------------------
        private def formatSubInfoDebug(value: SubInfo): DebugString =
            if (value.isSingle)                              formatValueTypeDebug(value.valueType)
            else                s"${formatMultiple(true)}\t${formatValueTypeDebug(value.valueType)}"

          // ---------------------------------------------------------------------------
          private def formatValueTypeDebug(value: ValueType): DebugString =
              value match {
                case tipe   : BasicType => tipe.entryName
                case nesting: Cls       => formatClassDebug(nesting).sectionAllOff }
}

// ===========================================================================
package gallia
package io
package out

// ===========================================================================
class EntirelyUriDrivenFluencyU(val conf: OutputConfU) extends EndWriteUFluency
class EntirelyUriDrivenFluencyZ(val conf: OutputConfZ) extends EndWriteZFluency

  // ===========================================================================
  class ForeachU(f: Obj => Unit) extends EndWriteUFluency { val conf = ForeachConfU(f) }
  class ForeachZ(f: Obj => Unit) extends EndWriteZFluency { val conf = ForeachConfZ(f) }

  // ===========================================================================
  class UriFluencyU(path: String) extends EndWriteUFluency { val conf = new UrlFluencyU(path).conf }
  class UriFluencyZ(path: String) extends EndWriteZFluency { val conf = new UrlFluencyZ(path).conf }

    // ===========================================================================
    class UrlFluencyU(path: String) extends EndWriteUFluency {
        val conf = OutputStringDrivenConf.urlLikeConfU(path)

        // ---------------------------------------------------------------------------
        def content = IoTypeU.RawContent       .urlLikeFluency(path)
        
        def json          = prettyJson
          def compactJson = IoTypeU.CompactJsonObject.urlLikeFluency(path)
          def prettyJson  = IoTypeU.PrettyJsonObject .urlLikeFluency(path)
      }

      // ===========================================================================
      class UrlFluencyZ(path: String) extends EndWriteZFluency {
        val conf = OutputStringDrivenConf.urlLikeConfZ(path)

        // ===========================================================================
        def lines = IoTypeZ.RawLines.urlLikeFluency(path)

        // ---------------------------------------------------------------------------
        def jsonl = jsonLines

          def jsonLines = IoTypeZ.JsonLines.urlLikeFluency(path)
          def jsonArray = IoTypeZ.JsonArray.urlLikeFluency(path)

        // ---------------------------------------------------------------------------
        def table(sep: Char): UrlLikeTableFluency = new UrlLikeTableFluency(UrlLikeTableConf(path).sep(sep))
          def tsv: UrlLikeTableFluency = table('\t')
          def csv: UrlLikeTableFluency = table(',')
def tsvwh: UrlLikeTableFluency = tsv.withHeader
def tsvnh: UrlLikeTableFluency = tsv.noHeader
        }

// ===========================================================================

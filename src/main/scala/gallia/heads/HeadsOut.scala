package gallia.heads

import aptus.Anything_
import aptus.UriString

import gallia.io.out._

// ===========================================================================
trait HeadUOut { _: HeadU =>

  def write()                                         : HeadU = write(x => x)
  def write(uri: UriString)                           : HeadU = write(_.entirelyUriDriven(uri))
  def write(f: StartWriteUFluency => EndWriteUFluency): HeadU = StartU.thn(f).conf.actionU.thn(uo)
    .sideEffect/*If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324 */ {
      _.end.runu().either match {
          case Left (errors)  => throw errors.metaErrorOpt.get
          case Right(success) => () } }

  // ===========================================================================
  // TODO: def printSchema()

  def formatString: String = formatJson

  def formatJson         : String = { formatPrettyJson }  
    def formatCompactJson: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).compactJson); sw.toString }
    def formatPrettyJson : String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).prettyJson) ; sw.toString }
  
  // ---------------------------------------------------------------------------
  def printString    () { printJson }
  
  def printJson         () { printPrettyJson }  
    def printCompactJson() { write(_.stdout.compactJson); () }
    def printPrettyJson () { write(_.stdout.prettyJson);  () }

  // ===========================================================================
  def writeFile(path: String) { write(_.file(path)); () }
  def writeDefaultFile { write(_.file(HeadU.DefaultOutputFile)); () }

  // TODO: pretty (print/write)
}

// ===========================================================================
trait HeadZOut { _: HeadZ =>

  def write()                                         : HeadZ = write(x => x)
  def write(uri: UriString)                           : HeadZ = write(_.entirelyUriDriven(uri))
  def write(uri: UriString, container: String)        : HeadZ = ??? // TODO: t201230105232
  def write(f: StartWriteZFluency => EndWriteZFluency): HeadZ = StartZ.thn(f).conf.actionZ.thn(zo)
    .sideEffect/*If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324 */ {
      _.end.runz().either match {
          case Left (errors)  => throw errors.metaErrorOpt.get
          case Right(success) => () } }

  // ===========================================================================
  def formatString: String = formatJsonLines
  //TODO: formatLines: Seq[Line]

  // ---------------------------------------------------------------------------
  def formatJsonLines: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).jsonLines); sw.toString }
  def formatJsonArray: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).jsonArray); sw.toString }

  // ===========================================================================
  //TODO: def printSchema()

  def printString   () { printJsonLines }

  def printJsonl    () { printJsonLines }
  def printJsonLines() { write(_.stdout.jsonLines); () }
  def printJsonArray() { write(_.stdout.jsonArray); () }

  // ---------------------------------------------------------------------------
  def printTable()       { write(_.stdout.tsv); () }
  def printPrettyTable() { write(_.stdout.tsv); () } // TODO: actually prettify

  // TODO: pretty (print/write)

  // ===========================================================================
  def writeFile(path: String) { write(_.file(path)); () }
  def writeDefaultFile { write(_.file(HeadZ.DefaultOutputFile)); () }
}

// ===========================================================================

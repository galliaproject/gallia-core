package gallia
package heads

import aptus.UriString

import io.out._

// ===========================================================================
trait HeadOut { self: Head[_] =>

  private[heads] def _meta: Unit =
    self.end.runMetaOnly().either match {        
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(success) => () }

}

// ===========================================================================
trait HeadUOut extends HeadOut { self: HeadU =>

  private[heads] def _all: Unit =
    self.end.runu().either match {
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(success) => () }

  // ===========================================================================
  def runGeneric(action: ActionUO): Unit = self.uo(action)._all

  // ---------------------------------------------------------------------------
  def write()                                         : HeadU = write(x => x)
  def write(uri: UriString)                           : HeadU = write(_.entirelyUriDriven(uri))
  def write(f: StartWriteUFluency => EndWriteUFluency): HeadU = StartU.pipe(f).conf.actionU.pipe(uo).tap(_._all) // If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324

  // ---------------------------------------------------------------------------
  private[gallia] def _metaOnly(): HeadU = StartU.pipe(_.stdout).conf.actionU.pipe(uo).tap(_._meta)

  // ===========================================================================
  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def printSchema() = { showSchema()._metaOnly() }
  
  // ---------------------------------------------------------------------------
  def formatString: String = formatJson

  def formatJson         : String = { formatPrettyJson }  
    def formatCompactJson: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).compactJson); sw.toString }
    def formatPrettyJson : String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).prettyJson) ; sw.toString }
  
  // ---------------------------------------------------------------------------
  def printDefault   () = { printJson }
  def printString    () = { printJson }
  
  def printJson         () = { printPrettyJson }  
    def printCompactJson() = { write(_.stdout.compactJson); () }
    def printPrettyJson () = { write(_.stdout.prettyJson);  () }

  // ---------------------------------------------------------------------------
  def printRow()       { self.convertToMultiple.printTable()      ; () }
  def printPrettyRow() { self.convertToMultiple.printPrettyTable(); () }    

  // ===========================================================================
  def writeFile(path: String) = { write(_.file(path)); () }
  def writeDefaultFile        = { write(_.file(HeadU.DefaultOutputFile)); () }

  // TODO: pretty (print/write)
  
  // ===========================================================================
  def display()                   : Unit = { display(forceRow = false) }
  def displayForceTable()         : Unit = { display(forceRow = true) }

  /** smart display: will show schema + choose table or JSON depending on whether there is nesting */
  def display(forceRow: Boolean): Unit = {
    printSchema()
    println()
    write(_.stdout.display(forceRow))
    ()
  }
  
  // ---------------------------------------------------------------------------
  def foreach(f: Obj => Unit) { write(_.foreach(f)); () }
}

// ===========================================================================
trait HeadZOut extends HeadOut { self: HeadZ =>

  private[heads] def _all: Unit =
    self.end.runz().either match {
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(success) => () }

  // ===========================================================================
  def runGeneric(action: ActionZO): Unit = self.zo(action)._all
  
  // ---------------------------------------------------------------------------
  def write()                                         : HeadZ = write(x => x)
  def write(uri: UriString)                           : HeadZ = write(_.entirelyUriDriven(uri))
  def write(uri: UriString, container: String)        : HeadZ = ??? // TODO: t201230105232
  def write(f: StartWriteZFluency => EndWriteZFluency): HeadZ = StartZ.pipe(f).conf.actionZ.pipe(zo).tap(_._all) // If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324      
  
  // ---------------------------------------------------------------------------
  private[gallia] def _metaOnly(): HeadZ = StartZ.pipe(_.stdout).conf.actionZ.pipe(zo).tap(_._meta)
  
  // ===========================================================================
  def formatString: String = formatJsonLines
  //TODO: formatLines: Seq[Line]

  // ---------------------------------------------------------------------------
  def formatJsonLines: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).jsonLines); sw.toString }
  def formatJsonArray: String = { val sw = new  java.io.StringWriter; write(_.formatString(sw).jsonArray); sw.toString }

  // ===========================================================================
  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def printSchema() = { showSchema()._metaOnly() }
  
  // ---------------------------------------------------------------------------
  def printDefault  () = { printJsonLines }
  def printString   () = { printJsonLines }

  def printJsonl      () = { printJsonLines }
  def printJsonLines  () = { write(_.stdout.jsonLines)  ; () }
  def printJsonArray  () = { write(_.stdout.jsonArray)  ; () }  
  def printPrettyJsons() = { write(_.stdout.prettyJsons); () }

  // ---------------------------------------------------------------------------
  def printTable()           = {          write(_.stdout.tsv); () }
  def printPrettyTable()     = {          write(_.stdout.prettyTable); () }
  def printPrettyTableHead() = { take(10).write(_.stdout.prettyTable); () }

  // ---------------------------------------------------------------------------
  def display()                   : Unit = { display(n = 10) }
  def display(n: Int)             : Unit = { display(n     , forceTable = false) }
  def display(forceTable: Boolean): Unit = { display(n = 10, forceTable) }
  def displayForceTable()         : Unit = { display(        forceTable = true) }

  /** smart display: will show schema + choose table or JSON depending on whether there is nesting */
  def display(n: Int, forceTable: Boolean): Unit = {
    printSchema()
    println()
    take(n).write(_.stdout.display(n, forceTable))
    ()
  }

  // ---------------------------------------------------------------------------
  def foreach(f: Obj => Unit) { write(_.foreach(f)); () }
  
  // ===========================================================================
  def writeFile(path: String) = { write(_.file(path)); () }
  def writeDefaultFile        = { write(_.file(HeadZ.DefaultOutputFile)); () }
  //TODO: t211220134905 - write pretty table?  
}

// ===========================================================================

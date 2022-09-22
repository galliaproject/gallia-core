package gallia
package heads

import aptus.{Nes, One, Opt, Pes, UriString}
import gallia.data.DataFormatting
import io.out._

// ===========================================================================
trait HeadOut { self: Head[_] =>

  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def forceSchema: Cls =
    self.end.runMetaOnly().either match {
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(success) => success.meta.forceLeafClass }

}

// ===========================================================================
trait HeadUOut extends HeadOut { self: HeadU =>

  private[heads] def _all: Unit =
    self.end.runu().either match {
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(_)       => () }

  // ===========================================================================
  def runGeneric(action: ActionUO): Unit = self.uo(action)._all

  // ---------------------------------------------------------------------------
  def write()                                         : HeadU = write(x => x)
  def write(uri: UriString)                           : HeadU = write(_.entirelyUriDriven(uri))
  def write(f: StartWriteUFluency => EndWriteUFluency): HeadU = StartU.pipe(f).conf.actionU.pipe(uo).tap(_._all) // If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324

  // ---------------------------------------------------------------------------
  def format(f: OtherFluencyU => EndWriteUFluency): String = { val sw = new java.io.StringWriter; write(_.formatString(sw).pipe(f)); sw.toString }
  
  // ---------------------------------------------------------------------------
  private[gallia] def _metaOnly(): HeadU = StartU.pipe(_.stdout).conf.actionU.pipe(uo).tap { x => x.forceSchema; () }

  // ===========================================================================
  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def formatSchema: String = forceSchema.formatDefault
  
  // ---------------------------------------------------------------------------
  def formatDefault   : String = formatJson
  def formatString    : String = formatJson
  
  def formatJson         : String = formatPrettyJson 
    def formatCompactJson: String = format(_.compactJson)
    def formatPrettyJson : String = format(_.prettyJson)

  // ---------------------------------------------------------------------------
  def formatRow        : String = self.convertToMultiple.formatTable
  def formatTable      : String = self.convertToMultiple.formatTable
  def formatPrettyRow  : String = self.convertToMultiple.formatPrettyTable
  def formatPrettyTable: String = self.convertToMultiple.formatPrettyTable

  // ===========================================================================
  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def printSchema() = { showSchema()._metaOnly() }
  
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
  
  // ===========================================================================
  def foreach(f: Obj => Unit) { write(_.foreach(f)); () }
}

// ===========================================================================
trait HeadZOut extends HeadOut { self: HeadZ =>

  private[heads] def _all: Unit =
    self.end.runz().either match {
      case Left (errors)  => throw errors.metaErrorOpt.get
      case Right(_)       => () }

  // ===========================================================================
  def runGeneric(action: ActionZO): Unit = self.zo(action)._all
  
  // ---------------------------------------------------------------------------
  def write()                                         : HeadZ = write(x => x)
  def write(uri: UriString)                           : HeadZ = write(_.entirelyUriDriven(uri))
  def write(uri: UriString, container: String)        : HeadZ = ??? // TODO: t201230105232
  def write(f: StartWriteZFluency => EndWriteZFluency): HeadZ = StartZ.pipe(f).conf.actionZ.pipe(zo).tap(_._all) // If(_.underlyingDagHasOnlyOutputLeaves /* 210205063004 */) - FIXME: t210122140324      
  
  // ---------------------------------------------------------------------------
  def format(f: OtherFluencyZ => EndWriteZFluency): String = { val sw = new java.io.StringWriter; write(_.formatString(sw).pipe(f)); sw.toString }
  
  // ---------------------------------------------------------------------------
  private[gallia] def _metaOnly(): HeadZ = StartZ.pipe(_.stdout).conf.actionZ.pipe(zo).tap { x => x.forceSchema; () }

  // ===========================================================================
  /** will *not* process all the data (assuming input schema does not need to be inferred) */
  def formatSchema: String = forceSchema.formatDefault    
  
  // ---------------------------------------------------------------------------  
  def formatString : String = formatJsonLines
  def formatDefault: String = formatJsonLines
  //TODO: formatLines: Seq[Line]

  // ---------------------------------------------------------------------------
  def formatJsonl      : String = format(_.jsonLines)  
  def formatJsonLines  : String = format(_.jsonLines)
  def formatJsonArray  : String = format(_.jsonArray)
  def formatPrettyJsons: String = format(_.prettyJsons)

  // ---------------------------------------------------------------------------
  def formatTable      : String = format(_.tsv)
  def formatPrettyTable: String = format(_.prettyTable)

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

  // ===========================================================================
  def display()                   : Unit = { display(max = 10) }
  def display(max: Int)           : Unit = { display(max     , forceTable = false) }
  def display(forceTable: Boolean): Unit = { display(max = 10, forceTable) }
  def displayForceTable()         : Unit = { display(          forceTable = true) }

  /** smart display: will show schema + choose table or JSON depending on whether there is nesting */
  def display(max: Int, forceTable: Boolean): Unit = {
    printSchema()
    println()
    take(max).write(_.stdout.display(forceTable))
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
trait HeadVOut[T] extends HeadOut { self: HeadV[T] => import data.DataDynamicFormatting.formatBasicValue

  private[heads] def forceValue  [T: WTT]: One[T] = end().runv[One[T]]().forceData2(_.value)
  private[heads] def forceValue_ [T: WTT]: Opt[T] = end().runv[Opt[T]]().forceData2(_.value)
  private[heads] def forceValues [T: WTT]: Nes[T] = end().runv[Nes[T]]().forceData2(_.value)
  private[heads] def forceValues_[T: WTT]: Pes[T] = end().runv[Pes[T]]().forceData2(_.value)

  // ===========================================================================
  private[heads] def _all: Unit =
    self.end.runv[T]().either match {
      case Left (errors) => throw errors.metaErrorOpt.get
      case Right(_)      => () }

  // ===========================================================================
  import actions.out.NakedValueOutput

    // ---------------------------------------------------------------------------
    def display()          : Unit = { println() }

    def write(path: String): HeadV[T] =                     { NakedValueOutput(Some(Left(path)), pair).pipe(vo).tap(_._all); self }
    def println()          : HeadV[T] =                     { NakedValueOutput(None,             pair).pipe(vo).tap(_._all); self }

    def format: String = { val sw = new java.io.StringWriter; NakedValueOutput(Some(Right(sw)) , pair).pipe(vo).tap(_._all); sw.toString }

    // ---------------------------------------------------------------------------
    private def pair: Vle => (Multiple, aptus.CloseabledIterator[aptus.StringValue]) = {
        case seq: Seq[_] => _Multiple -> seq.iterator.map(formatBasicValue).pipe(aptus.CloseabledIterator.fromUncloseable(_))
        case sgl         => _Single   -> Seq(sgl)    .map(formatBasicValue).pipe(aptus.CloseabledIterator.fromSeq(_)) }
}

// ===========================================================================

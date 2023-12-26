package gallia
package domain

import aptus.{Anything_, Seq_}

import heads.Head

// ===========================================================================
private[gallia] case class AObj(c: Cls, @deprecated u: Obj) { // TODO: tt210124100009 - initially stood for "Annotated Object"...
            def schema = c
            def data   = u    
    @inline def o      = u

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        o.pp

    def toBObj : BObj  = ??? //TODO
    def inAObjs: AObjs = AObjs(c, Objs.from(List(o))) }

  // ---------------------------------------------------------------------------
  private[gallia] object AObj {
    implicit def toHead(value: AObj): HeadU =
      actions.in
        .InMemoryInputUa(value)
        .pipe(Head.inputU) }

// ===========================================================================
private[gallia] case class AObjs(c: Cls, z: Objs) {
    def schema = c
    def data   = z

    // ---------------------------------------------------------------------------
    def forceAObj: AObj = AObj(c, z.toListAndTrash.force.one)
    
    def rawData: Iterator[Seq[AnyValue]] = z.consumeSelfClosing.map(_.values)

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        z.formatPrettyJson }

  // ===========================================================================
  private[gallia] object AObjs {
    implicit def toHead(value: AObjs): HeadZ =
      actions.in
        .InMemoryInputZa(value)
        .pipe(Head.inputZ)

    // ---------------------------------------------------------------------------
    def combineCls(values: Seq[Cls]): Cls = values.reduceLeft(_ unionCompatible _)

    // ---------------------------------------------------------------------------
    def from(values: Seq[AObj]): AObjs =
      AObjs(
        c = values.map(_.c).pipe(combineCls),
        z = values.map(_.o).toList.pipe(Objs.from))

    // ---------------------------------------------------------------------------
    lazy val Empty             : AObjs = empty(Cls.Line)
         def empty(schema: Cls): AObjs = AObjs(schema, Objs.empty) }

// ===========================================================================

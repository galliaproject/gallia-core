package gallia
package domain

import aptus.{Anything_, Seq_}

import heads.Head

// ===========================================================================
case class AObj(c: Cls, @deprecated u: Obj) { // TODO: tt210124100009 - initially stood for "Annotated Object"...
            def schema = c
            def data   = u    
    @inline def o      = u

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        u.pp

    def toBObj : BObj  = ??? //TODO
    def inAObjs: AObjs = AObjs(c, Objs.from(List(o)))
  }

  // ---------------------------------------------------------------------------
  object AObj {
    implicit def toHead(value: AObj): HeadU =
      new actions.in
        .InMemoryInputUa(value)
        .pipe(Head.inputU)
  }

// ===========================================================================
case class AObjs(c: Cls, z: Objs) {
    def schema = c
    def data   = z
    
    // ---------------------------------------------------------------------------
    def forceAObj: AObj = AObj(c, z.toListAndTrash.force.one)
    
    def rawData: Iterator[Seq[AnyValue]] = z.consume.map(_.values)

    // ---------------------------------------------------------------------------
    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        z.formatPrettyJson
  }

  // ===========================================================================
  object AObjs {
    implicit def toHead(value: AObjs): HeadZ =
      new actions.in
        .InMemoryInputZa(value)
        .pipe(Head.inputZ)

    // ---------------------------------------------------------------------------
    def combineCls(values: Seq[Cls]): Cls = values.reduceLeft(_ unionCompatible _)

    def from(values: Seq[AObj]): AObjs =
      AObjs(
        c = values.map(_.c).pipe(combineCls),
        z = values.map(_.o).toList.pipe(Objs.from))
  }


// ===========================================================================

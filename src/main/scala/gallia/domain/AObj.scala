package gallia.domain

import aptus.Anything_

import gallia._
import gallia.heads.Head

// ===========================================================================
case class AObj (c: Cls, u: Obj ) { // TODO: tt210124100009 - initially stood for "Annotated Object"...

    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        u.pp

    def toBObj: BObj = ??? //TODO
  }

  // ---------------------------------------------------------------------------
  object AObj {
    implicit def toHead(value: AObj): HeadU =
      new gallia.actions.in
        .InMemoryInputUa(value)
        .thn(Head.inputU)
  }

// ===========================================================================
case class AObjs(c: Cls, z: Objs) {

    override def toString: String = formatDefault
      def formatDefault: String =
        c.formatDefault + "\n" +
        z.formatPrettyJson
  }

  // ===========================================================================
  object AObjs {
    implicit def toHead(value: AObjs): HeadZ =
      new gallia.actions.in
        .InMemoryInputZa(value)
        .thn(Head.inputZ)

    // ---------------------------------------------------------------------------
    def combineCls(values: Seq[Cls]): Cls = values.reduceLeft(_ unionCompatible _)

    def from(values: Seq[AObj]): AObjs =
      AObjs(
        c = values.map(_.c).thn(combineCls),
        z = values.map(_.u).thn(Objs.from))
  }


// ===========================================================================

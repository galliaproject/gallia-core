package gallia.domain

import aptus.Anything_

import gallia._
import gallia.heads.Head

// ===========================================================================
case class BObj private[gallia](entries: gallia.domain.KVEs) { // TODO: t210124100009 - no proper rationale for "B" prefix

    override def toString: String = formatDefault
      def formatDefault: String = entries.formatDefault

    // ---------------------------------------------------------------------------
    def keys = entries.keys

    def either: Either[Errs, AObj] =
      vldt.MetaValidation.validateBObj(this) match {
        case Nil => Right(forceAObj)
        case seq => Left(seq) }

    def forceCls: Cls = entries.forceCls
    def forceObj: Obj = entries.forceObj

    def forceAObj: AObj = AObj(forceCls, forceObj)
  }

  // ===========================================================================
  object BObj {
    implicit def toHead(value: BObj): HeadU =
      new gallia.actions.in
        .InMemoryInputUb(value)
        .pipe(Head.inputU)
  }

// ===========================================================================
case class BObjs(values: Seq[BObj]) {
    def forceCls : Cls  = values.map(_.forceCls).pipe(AObjs.combineCls)
    def forceObjs: Objs = values.map(_.forceObj).pipe(Objs.from)
    def forceAObjs: AObjs = AObjs.from(values.map(_.forceAObj))
  }

  // ---------------------------------------------------------------------------
  object BObjs {
    implicit def toHead(value: BObjs): HeadZ =
      new gallia.actions.in
        .InMemoryInputZb(value)
        .pipe(Head.inputZ)
  }

// ===========================================================================

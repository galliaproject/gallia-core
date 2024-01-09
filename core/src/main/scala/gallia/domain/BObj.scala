package gallia
package domain

import heads.Head

// ===========================================================================
private[gallia] case class BObj private[gallia](entries: KVEs) { // TODO: t210124100009 - no proper rationale for "B" prefix

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

    def forceAObj: AObj = AObj(forceCls, forceObj) }

  // ===========================================================================
  private[gallia] object BObj {
    implicit def toHead(value: BObj): HeadU =
      actions.in
        .InMemoryInputUb(value)
        .pipe(Head.inputU) }

// ===========================================================================
private[gallia] case class BObjs(values: Seq[BObj]) {
    def forceCls : Cls  = values.map(_.forceCls)       .pipe(AObjs.combineCls)
    def forceObjs: Objs = values.map(_.forceObj).toList.pipe(Objs.from)
    def forceAObjs: AObjs = AObjs.from(values.map(_.forceAObj)) }

  // ---------------------------------------------------------------------------
  private[gallia] object BObjs {
    implicit def toHead(value: BObjs): HeadZ =
      actions.in
        .InMemoryInputZb(value)
        .pipe(Head.inputZ) }

// ===========================================================================

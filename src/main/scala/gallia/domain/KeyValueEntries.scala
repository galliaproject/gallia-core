package gallia.domain

import aptus.{Anything_, Seq_}
import aptus.Tuple2_

import gallia._
import gallia.vldt._
import gallia.reflect.TypeNode
import gallia.meta.{Fld => _, _}

// ===========================================================================
sealed trait KVE { // Key-Value Entry
    val key: Key

    def formatDefault: String = toString

    def vldt(nextLocation: Location): Errs
    def metaEntry: (Key, Info)
    def dataEntry: (Key, AnyValue)
  }

  // ===========================================================================
  case class BObjKVE(key: Key, bobj: BObj) extends KVE {
      def vldt(nextLocation: Location): Errs = MetaValidation.validateBObj(nextLocation)(bobj)
      def metaEntry: (Key, Info)     = key -> bobj.forceCls.thn(Info.one)
      def dataEntry: (Key, AnyValue) = key -> bobj.forceObj
    }

  // ===========================================================================
  case class BObjsKVE(key: Key, bobjs: Seq[BObj]) extends KVE {
      def vldt(nextLocation: Location): Errs =
        bobjs
          .zipWithIndex
          .flatMap { case (value, index) =>
            MetaValidation.validateBObj(
                nextLocation.addIndex(index))(value) }

      @gallia.NumberAbstraction
      def metaEntry: (Key, Info)     = key -> bobjs.map(_.forceCls).distinct.force.one.thn(Info.nes)

      def dataEntry: (Key, AnyValue) = key -> bobjs.map(_.forceObj)
    }

  // ===========================================================================
  case class VleKVE(key: Key, node: TypeNode, value: AnyValue) extends KVE {

      def vldt(nextLocation: Location): Errs =
        node
          .validContainerOpt
          .flatMap(_.as.someIf(_.dataClass))
           match {
            case None     => MetaValidation.validType        (nextLocation, node)
            case Some(cc) => MetaValidation.validateCaseClass(nextLocation)(cc) }

      def metaEntry: (Key, Info) = key -> node.forceNonBObjInfo

      def dataEntry: (Key, AnyValue) = key ->
        (node
          .forceNonBObjInfo
          .nestingTypeOpt
          .map { c =>
              if (node.isMultiple) c.valueToObjs(value)
              else                 c.valueToObj (value) }
          .getOrElse(value) )
    }

  // ===========================================================================
  object KVE {
    // TODO: t210125111338 - investigate union types to restrict T (coming in scala 3?)

    implicit def toValueEntryK[T: WTT](x: ( Key, T)): KVE = toValueEntry(x._1, x._2)
    implicit def toValueEntryS[T: WTT](x: (SKey, T)): KVE = toValueEntry(x._1, x._2)
    implicit def toValueEntryU[T: WTT](x: (UKey, T)): KVE = toValueEntry(x._1, x._2)
    implicit def toValueEntryE[T: WTT](x: (EKey, T)): KVE = toValueEntry(x._1, x._2)

    // ---------------------------------------------------------------------------
    implicit def toValueEntryK[T: WTT](k:  Key, v: T): KVE = toValueEntry(k, v)
    implicit def toValueEntryS[T: WTT](k: SKey, v: T): KVE = toValueEntry(k, v)
    implicit def toValueEntryU[T: WTT](k: UKey, v: T): KVE = toValueEntry(k, v)
    implicit def toValueEntryE[T: WTT](k: EKey, v: T): KVE = toValueEntry(k, v)

    // ---------------------------------------------------------------------------
    implicit def toValueEntry[T: WTT](key: KeyW, value: T): KVE = {
      val _node = node[T]

           if (node.isBObj ) BObjKVE (key.value, value.asInstanceOf[    BObj ])
      else if (node.isBObjs) BObjsKVE(key.value, value.asInstanceOf[Seq[BObj]])
      else                   VleKVE  (key.value, _node, value)
    }

  }

// ===========================================================================
case class RVE(underlying: KVE, to: Key) {
    def ren: Ren = Ren(underlying.key, to)

    override def toString: String = formatDefault
      def formatDefault: String = if (underlying.key != to) s"${underlying.key} ~> ${underlying.formatDefault}" else underlying.formatDefault

    def metaEntry: (Ren, Info    ) = underlying.metaEntry.mapFirst(_ => ren)
    def dataEntry: (Ren, AnyValue) = underlying.dataEntry.mapFirst(_ => ren)
  }

  // ---------------------------------------------------------------------------
  object RVE {
    implicit def to1[T : WTT](x: ( Key, T)): RVE = to(x._1, x._2)
    implicit def to2[T : WTT](x: (SKey, T)): RVE = to(x._1, x._2)
    implicit def to3[T : WTT](x: (UKey, T)): RVE = to(x._1, x._2)
    implicit def to4[T : WTT](x: (EKey, T)): RVE = to(x._1, x._2)
    //TODO:?

    implicit def to[T : WTT](key: RenW, value: T): RVE = RVE((key.from, value), key.to)

    implicit def toValueEntry5[T : WTT](x: (Ren, T)): RVE = RVE((x._1.from, x._2), x._1.to)
  }

// ===========================================================================
case class KVEs(values: Seq[KVE]) {
    override def toString: String = formatDefault
      def formatDefault: String = values.map(_.formatDefault).joinln

    // ---------------------------------------------------------------------------
    def keys: Seq[Key] = values.map(_.key)
    def keyz: Keyz = Keyz(values.map(_.key))

    // ---------------------------------------------------------------------------
    def forceMetaEntries: Seq[(Key, Info    )] = values.map(_.metaEntry)
    def forceDataEntries: Seq[(Key, AnyValue)] = values.map(_.dataEntry)

    // ---------------------------------------------------------------------------
    def forceCls: Cls = forceMetaEntries.map((Fld.apply _).tupled).thn(gallia.meta.Cls(_))
    def forceObj: Obj = forceDataEntries.thn(gallia.obj)

  }

  // ===========================================================================
  object KVEs {
    implicit def from[T1: WTT](e1: KVE) = KVEs(Seq(e1))

      implicit def from[T1: WTT, T2: WTT                           ](t: (KVE, KVE               )) = KVEs(Seq(t._1, t._2))
      implicit def from[T1: WTT, T2: WTT, T3: WTT                  ](t: (KVE, KVE, KVE          )) = KVEs(Seq(t._1, t._2, t._3))
      implicit def from[T1: WTT, T2: WTT, T3: WTT, T4: WTT         ](t: (KVE, KVE, KVE, KVE     )) = KVEs(Seq(t._1, t._2, t._3, t._4))
      implicit def from[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](t: (KVE, KVE, KVE, KVE, KVE)) = KVEs(Seq(t._1, t._2, t._3, t._4, t._5))
  }

// ===========================================================================
case class RVEs(values: Seq[RVE]) {
    override def toString: String = formatDefault
      def formatDefault: String = values.map(_.formatDefault).joinln

    // ---------------------------------------------------------------------------
    def kves = KVEs(values.map(_.underlying))

    def renz: Renz = values.map(_.ren).thn(Renz.apply)

    // ---------------------------------------------------------------------------
    def forceMetaEntries: Seq[(Ren, Info    )] = values.map(_.metaEntry)
    def forceDataEntries: Seq[(Ren, AnyValue)] = values.map(_.dataEntry)
  }

  // ===========================================================================
  object RVEs {
    implicit def from[T1: WTT](e1: RVE) = RVEs(Seq(e1))

      implicit def from[T1: WTT, T2: WTT                           ](t: (RVE, RVE               )) = RVEs(Seq(t._1, t._2))
      implicit def from[T1: WTT, T2: WTT, T3: WTT                  ](t: (RVE, RVE, RVE          )) = RVEs(Seq(t._1, t._2, t._3))
      implicit def from[T1: WTT, T2: WTT, T3: WTT, T4: WTT         ](t: (RVE, RVE, RVE, RVE     )) = RVEs(Seq(t._1, t._2, t._3, t._4))
      implicit def from[T1: WTT, T2: WTT, T3: WTT, T4: WTT, T5: WTT](t: (RVE, RVE, RVE, RVE, RVE)) = RVEs(Seq(t._1, t._2, t._3, t._4, t._5))
  }

// ===========================================================================

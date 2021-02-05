package gallia.meta

import aptus.Anything_

import gallia._
import gallia.heads.reducing.ReducingType

// ===========================================================================
trait ClsAggregating { self: Cls =>

  def group1N(groupee: Ren, groupers: Renz): Cls =
      (retain(groupers), field(groupee))
        .thn { case (a, b) => a.fields :+ b.toMultiple }
        .thn(Cls.apply)

    def groupNN(groupees: Renz, groupers: Renz, as: Key): Cls =
      (retain(groupers), retain(groupees))
        .thn { case (a, b) => a.fields :+ Fld.nesCls(as, b) }
        .thn(Cls.apply)

  // ===========================================================================
  @gallia.IntSize
  def countAll(groupers: Renz, as: Key): Cls = // independent of groupee
      Cls(
        retain(groupers).fields :+
        Fld.oneInt(as))

  // ===========================================================================
  // TODO: reuse newer reducer code rather?

  // ---------------------------------------------------------------------------
  def aggregate1(rtipe: ReducingType)(groupee: Ren, groupers: Renz): Cls =
      Cls(
          retain(groupers).fields :+
          rtipe.field(groupee.to, original = field(groupee.from)) )

    // ---------------------------------------------------------------------------
    //FIXME: t210202163542 - if no agg?
    def aggregateN(rtipe: ReducingType)(groupers: Renz, groupees: Renz, as: Key): Cls =
      Cls(
          retain(groupers).fields :+
          Fld(as, Info.one(retain(groupees).mapFields(_.toNonMultiple) )) )


    // ---------------------------------------------------------------------------
    def stats(groupers: Renz, as: Key): Cls = // TODO: revamp, see t210118084355
      Cls(
          retain(groupers).fields :+
          Fld.oneCls(as, Cls.FullDescriptiveStats))

}

// ===========================================================================

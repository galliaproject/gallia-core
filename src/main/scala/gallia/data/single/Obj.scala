package gallia.data.single

import aptus.{Anything_, String_, Seq_}
import aptus.json.GsonFormatter
import gallia.data.json.ObjToGson

// ===========================================================================
case /* for equality */ class Obj private (protected[data] val data: UData) // TODO: two versions, see t210104164036
      extends ObjAccessors
      with    ObjOperations {
      // TODO: extends Ordered?

    if (!gallia.CheatMode) { // t210107094406 - possibility to opt out of the checks (for performance) - should opt out by default in prod

      // TODO: should use Key, Ren, ... directly for performance
      // ---------------------------------------------------------------------------
      if (data.isEmpty)
        throw gallia.ObjCantBeEmpty

      if (!keys.isDistinct) // a201026170344
        throw new gallia.RuntimeError("DuplicateKeys: " + entries.map(_._1).duplicates.#@@)

      // ---------------------------------------------------------------------------
      require( // a201104150252
          !data.values.exists(_.isInstanceOf[gallia.Objs]),
          (data.filter(_._2.isInstanceOf[gallia.Objs]).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain Objz: "))

      require( // a201104150253
          !data.values.exists(_.isInstanceOf[Option[_]]), // TODO: efficiency...
          (data.filter(_._2    .isInstanceOf[Option[_]]).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain Option: "))

      require( // a201104150254
          !data.values.exists(_ == Seq()), // TODO: efficiency...
          (data.filter(_._2     == Seq()).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain empty Seq: "))

      require( // a201113115339
           data.forall(x => if (x._2.isInstanceOf[Seq[_]]) x._2.asInstanceOf[Seq[_]].forall(!_.isInstanceOf[Iterable[_]]) else true),
          (data.filter(x =>     x._2.isInstanceOf[Seq[_]]).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain Seq of Iterable (eg Seq or Option): "))

      //TODO:
      // - t210203124840 - also check value types (from BasicType)
      // - t210203124717 - check for NaN
    }

    // ===========================================================================
    override def toString: String = formatDefault

      def formatDefault: String = formatCompactJson

    def formatPrettyJson  : String = ObjToGson(this).thn(GsonFormatter.pretty)
    def formatCompactJson : String = ObjToGson(this).thn(GsonFormatter.compact)

    // ===========================================================================
    def keys   : Seq[ Key           ] = data.keys  .toList
    def values : Seq[      AnyValue ] = data.values.toList
    def entries: Seq[(Key, AnyValue)] = data       .toList

    // ---------------------------------------------------------------------------
    final      def  keyz  :      Keyz = Keyz(keys)
    final lazy val  keySet: Set[ Key] = keys.toSet
    final lazy val skeySet: Set[SKey] = data.keys.map(_.name).toSet

    // ===========================================================================
    //TODO: t210110203020 - contains needs to be able to deal with multiplicity in nesting(s)
    //TODO: optimize
    def contains   (path: KPathW): Boolean = opt(path).isDefined
    def containsNot(path: KPathW): Boolean = opt(path).isEmpty

    // ===========================================================================
    def potch(key: Key): (Option[AnyValue], Option[Obj]) = opt(key) -> removeOpt(key) // totally a thing. (TODO: t210124100009)

      // ---------------------------------------------------------------------------
      def retainOpt(target : Key)  : Option[Obj] = data.filter   (_._1 == target).as.noneIf(_.isEmpty).map(Obj.build)
      def removeOpt(target : Key)  : Option[Obj] = data.filterNot(_._1 == target).as.noneIf(_.isEmpty).map(Obj.build)

      // ---------------------------------------------------------------------------
      def retainOpt(targets: Keyz): Option[Obj] = data.filter   (_._1.containedIn(targets)).as.noneIf(_.isEmpty).map(Obj.build)
      def removeOpt(targets: Keyz): Option[Obj] = data.filterNot(_._1.containedIn(targets)).as.noneIf(_.isEmpty).map(Obj.build)
  }

  // ===========================================================================
  object Obj {
    def content(value: String)  : Obj = gallia.obj(gallia._content -> value)
    def line   (value: String)  : Obj = gallia.obj(gallia._line    -> value)
    def array  (value: Seq[Obj]): Obj = gallia.obj(gallia._array   -> value.toList)

    // ---------------------------------------------------------------------------
    /* must not expose apply: see 210102140902 */
    private[gallia] def build   (data: UData):        Obj  = new Obj(ObjIn.normalize(data))
    private[gallia] def buildOpt(data: UData): Option[Obj] = ObjIn.normalize(data).as.noneIf(_.isEmpty).map(new Obj(_))
  }

// ===========================================================================

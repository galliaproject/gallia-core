package gallia.data.single

import aptus.{Anything_, String_, Seq_}
import aptus.json.GsonFormatter
import gallia.data.json.ObjToGson

// ===========================================================================
class Obj private ( /* must not expose apply: see 210102140902, mostly so can use .obj(...) accessor */   
        protected[data] val data: UData) // TODO: two versions, see t210104164036
      extends ObjAccessors
      with    ObjOperations { // TODO: extends Ordered?
    @inline @deprecated def _data = data

    // ===========================================================================
    override def hashCode: Int = data.toList.hashCode() // TODO: costly?

    // ---------------------------------------------------------------------------
    override def equals(that: Any): Boolean = that match {
        case that: Obj => { 
          this.data.toList.sortBy(_._1.name) == // TODO: t210611121237 - not relying on sorting...
          that.data.toList.sortBy(_._1.name) }
        case _ => false }

    // ===========================================================================
    if (!gallia.Hacks.DisableRuntimeChecks) { // t210107094406 - possibility to opt out of the checks (for performance) - should opt out by default in prod

      // TODO: should use Key, Ren, ... directly for performance
      // ---------------------------------------------------------------------------
      if (data.isEmpty)     gallia.vldt._Error.ObjCantBeEmpty                   .throwRuntimeError()
      if (!keys.isDistinct) gallia.vldt._Error.ObjDuplicateKeys(keyz.duplicates).throwRuntimeError()

      // ---------------------------------------------------------------------------
      // TODO: to proper errors
      require( // a201104150252
          !data.map   (_._2).exists(_.isInstanceOf[gallia.Objs]),
          (data.filter(_._2          .isInstanceOf[gallia.Objs]).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain Objz: "))

      require( // a201104150253
          !data.map   (_._2).exists(_.isInstanceOf[Option[_]]), // TODO: efficiency...
          (data.filter(_._2          .isInstanceOf[Option[_]]).map(_._1).toSeq.#@@, this)
            .str.prepend("can't contain Option: "))

      require( // a201104150254
          !data.map   (_._2).exists(_ == Seq()), // TODO: efficiency...
          (data.filter(_._2           == Seq()).map(_._1).toSeq.#@@, this)
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
    def keys   : Seq[ Key           ] = data.map(_._1).toList
    def values : Seq[      AnyValue ] = data.map(_._2).toList
    def entries: Seq[(Key, AnyValue)] = data          .toList

    // ---------------------------------------------------------------------------
    final      def  keyz  :      Keyz = Keyz(keys)
    final lazy val  keySet: Set[ Key] = keys.toSet
    final lazy val skeySet: Set[SKey] = data.map(_._1).map(_.name).toSet

    // ---------------------------------------------------------------------------
    private[single] def attemptKey (key: Key): Option[AnyValue] = data.find  (_._1 == key).map(_._2)    
    private[single] def containsKey(key: Key):        Boolean   = data.exists(_._1 == key)
            
    // ===========================================================================
    def contains   (path: KPathW): Boolean =  _contains(path)
    def containsNot(path: KPathW): Boolean = !_contains(path)

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
  object Obj { import ObjIn.normalize
    
    // ---------------------------------------------------------------------------
    def content(value: String)  : Obj = gallia.obj(gallia._content -> value)
    def line   (value: String)  : Obj = gallia.obj(gallia._line    -> value)
    def array  (value: Seq[Obj]): Obj = gallia.obj(gallia._array   -> value.toList)

    // ---------------------------------------------------------------------------
    private[gallia] def build0      (data: UData)           : Obj = new Obj(data)
    private[gallia] def build       (data: UData)           : Obj = build0(normalize(data))
    private[gallia] def fromIterable(data: Iterable[UEntry]): Obj = build (data.toArray)
  }

// ===========================================================================

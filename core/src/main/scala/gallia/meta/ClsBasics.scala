package gallia
package meta

import aptus.Anything_

// ===========================================================================
trait ClsBasics { self: Cls =>

  def rename(from: Key  , to: Key): Cls = _rename(from, to)
  def rename(from: KPath, to: Key): Cls = transformx(from)(_.rename(_, to), _.rename(_, to))

  def rename(entry: ActualRen): Cls = rename(entry.from, entry.to)

  def rename(entry: Ren      ): Cls = if (entry.isActual) rename(entry.from, entry.to) else this
  def rename(keyz : Renz     ): Cls = keyz.foldLeft(this)(_ rename _)

  def rename(path : RPath    ): Cls = transformx(path)(_ rename _, _ rename _)
  def rename(pathz: RPathz   ): Cls = pathz.foldLeft(this)(_ rename _)

  // ===========================================================================
  def put(pair: (KPath, Info))(implicit di: DummyImplicit): Cls = put(pair._1, pair._2)
  def put(path: KPath, info: Info): Cls =
    if (contains(path)) replace(path, info)
    else                add    (path, info)

  // ---------------------------------------------------------------------------
  def put(pair: (Key, Info)): Cls = put(pair._1, pair._2)
  def put(key: Key, info: Info): Cls =
    if (contains(key)) replace(key, info)
    else               add    (key, info)

  def putField(fld: Fld): Cls = put(fld.key, fld.info)

  // ---------------------------------------------------------------------------
  def replace(path: RPath, info: Info): Cls = transformx(path)(_.replace(_, info), _.replace(_, info))

  def replace(pair: (Ren, Info))                 : Cls = replace(pair._1, pair._2)
  def replace(pair: (Key, Info))(implicit di: DI): Cls = replace(pair._1, pair._2)

  def replace(key: Key, info: Info)              : Cls = replace(Ren.from(key), info)
  def replace(key: Ren, info: Info)              : Cls = _replace(key, info)

  def replace(path: KPath, info: Info): Cls = transformx(path)(_.replace(_, info), _.replace(_, info))

  def replace(pairs: Seq[(Ren, Info)])                 : Cls = pairs.foldLeft(this)(_ replace _)
  def replace(pairs: Seq[(Key, Info)])(implicit di: DI): Cls = pairs.foldLeft(this)(_ replace _)

  // ---------------------------------------------------------------------------
  def add      (field: Fld)             : Cls = { requireNewKey(field.key); rewrap(fields :+ field) }
  def addField (field: Fld)             : Cls = { requireNewKey(field.key); rewrap(fields :+ field) }
  def addBefore(field: Fld, target: Key): Cls = { requireNewKey(field.key); val index = indexOf(target).assert(_ >= 0); rewrap(fields.take(index)     ++ List(field) ++ fields.drop(index)) }
  def addAfter (field: Fld, target: Key): Cls = { requireNewKey(field.key); val index = indexOf(target).assert(_ >= 0); rewrap(fields.take(index + 1) ++ List(field) ++ fields.drop(index + 1)) }

  def add(pair: (Key, Info))   : Cls = add(Fld(pair._1, pair._2))
  def add(key: Key, info: Info): Cls = add(Fld(key, info))

  def add(path: KPath, info: Info)             : Cls = transformx(path)(_.add(_, info), _.add(_, info))
  def add(pair: (KPath, Info))(implicit di: DI): Cls = add(pair._1, pair._2)

  def add(pairs: Seq[(Key, Info)])             : Cls = pairs.foldLeft(this)(_ add _)

  def add(field1: Fld, field2: Fld, more: Fld*): Cls = Cls(this.fields ++ (Seq(field1, field2) ++ more).toList)

  // ===========================================================================
  @deprecated("favor combo retain+rename") def retain(key : Ren) : Cls = { requireRenamingKey(key); rewrap(field(key).in.seq) }
  @deprecated("favor combo retain+rename") def retain(keys: Renz): Cls = retain(keys.froms).pipe { x => keys.foldLeft(x)(_ rename _) }

  def retain(value: Key) : Cls = rewrap(field(value).in.seq)
  def retain(value: KeyW): Cls = retain(value.value)

  def retain(keys: Keyz): Cls = rewrap(fields.filter(field => keys.values.contains(field.key)))
  def retain(paths: KPathz): Cls = {
    val mapping: Map[Key, Option[KPathz]]= paths.mapping

    keys
      .flatMap { key =>
        mapping.get(key).map {
          case None              => field(key)
          case Some(nestedPaths) => field(key).transformAllSubInfos(_.transformNestedClass(_.retain(nestedPaths))) } }
      .pipe(Cls.apply)
  }

  // ===========================================================================
  def remove(key: Key): Cls = { requireKnownKey(key); rewrap(fields.filterNot(_.key == key)) }

  def remove     (keys: Keyz   ): Cls = remove(keys.values)
  def remove     (keys: Seq[Key]): Cls = keys.foldLeft(this)(_ remove _)

  def remove     (path: KPath): Cls = transformx(path)(_ remove _, _ remove _)
  def remove     (path: KPathW): Cls = transformx(path.value)(_ remove _, _ remove _)
  def removeField(path: KPathW): Cls = transformx(path.value)(_ remove _, _ remove _)

  def remove(paths: KPathz): Cls = paths.foldLeft(this)(_ remove _)
}

// ===========================================================================

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
  def put(pair: (KPath, Ofni))(implicit di: DummyImplicit): Cls = put(pair._1, pair._2)
  def put(path: KPath, ofni: Ofni): Cls =
    if (contains(path)) replace(path, ofni)
    else                add    (path, ofni)

  // ---------------------------------------------------------------------------
  def put(pair: (Key, Ofni)): Cls = put(pair._1, pair._2)
  def put(key: Key, ofni: Ofni): Cls =
    if (contains(key)) replace(key, ofni)
    else               add    (key, ofni)

  // ---------------------------------------------------------------------------
  def replace(path: RPath, ofni: Ofni): Cls = transformx(path)(_.replace(_, ofni), _.replace(_, ofni))

  def replace(pair: (Ren, Ofni))                 : Cls = replace(pair._1, pair._2)
  def replace(pair: (Key, Ofni))(implicit di: DI): Cls = replace(pair._1, pair._2)

  def replace(key: Key, ofni: Ofni)              : Cls = replace(Ren.from(key), ofni)
  def replace(key: Ren, ofni: Ofni)              : Cls = _replace(key, ofni)

  def replace(path: KPath, ofni: Ofni): Cls = transformx(path)(_.replace(_, ofni), _.replace(_, ofni))

  def replace(pairs: Seq[(Ren, Ofni)])                 : Cls = pairs.foldLeft(this)(_ replace _)
  def replace(pairs: Seq[(Key, Ofni)])(implicit di: DI): Cls = pairs.foldLeft(this)(_ replace _)

  // ---------------------------------------------------------------------------
  def add(field: Fld): Cls = { requireNewKey(field.key); rewrap(fields :+ field) }

  def add(pair: (Key, Ofni))   : Cls = add(Fld(pair._1, pair._2))
  def add(key: Key, ofni: Ofni): Cls = add(Fld(key, ofni))

  def add(path: KPath, ofni: Ofni)             : Cls = transformx(path)(_.add(_, ofni), _.add(_, ofni))
  def add(pair: (KPath, Ofni))(implicit di: DI): Cls = add(pair._1, pair._2)

  def add(pairs: Seq[(Key, Ofni)])             : Cls = pairs.foldLeft(this)(_ add _)

  // ===========================================================================
  @deprecated("favor combo retain+rename") def retain(key : Ren) : Cls = { requireRenamingKey(key); rewrap(field(key).in.seq) }
  @deprecated("favor combo retain+rename") def retain(keys: Renz): Cls = retain(keys.froms).pipe { x => keys.foldLeft(x)(_ rename _) }

  def retain(value: Key  ): Cls = rewrap(field(value).in.seq)
  def retain(value: KeyW): Cls = retain(value.value)

  def retain(keys: Keyz): Cls = rewrap(fields.filter(field => keys.values.contains(field.key)))
  def retain(paths: KPathz): Cls = {
    val mapping: Map[Key, Option[KPathz]]= paths.mapping

    keys
      .flatMap { key =>
        mapping.get(key).map {
          case None              => field(key)
          case Some(nestedPaths) => field(key).transformAllInfos(_.transformNestedClass(_.retain(nestedPaths))) } }
      .pipe(Cls.apply)
  }

  // ===========================================================================
  def remove(key: Key): Cls = { requireKnownKey(key); rewrap(fields.filterNot(_.key == key)) }

  def remove(keys: Keyz   ): Cls = remove(keys.values)
  def remove(keys: Seq[Key]): Cls = keys.foldLeft(this)(_ remove _)

  def remove(path: KPath): Cls = transformx(path)(_ remove _, _ remove _)

  def remove(paths: KPathz): Cls = paths.foldLeft(this)(_ remove _)
}

// ===========================================================================

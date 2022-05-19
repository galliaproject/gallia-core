package gallia
package atoms
package common

import aptus.{String_, Seq_, Tuple2_}

// ===========================================================================
object AtomsUUUntuplify {

  def untuplify1z(o: Obj, targetKey: Ren, keys: Keyz): Obj =
    o .potch(targetKey.from)
      .mapFirst(_.map(_.asInstanceOf[Seq[_]].map(_.toString)))
      .pipe { case (targetOpt, restOpt) =>
        targetOpt
          .map(_untuplify1z(keys))
          .map(update(restOpt, targetKey.to))
          .getOrElse(o) }

    // ---------------------------------------------------------------------------
    private def _untuplify1z(keys: Keyz)(values: Seq[Any]): Obj = {
      if (keys.size != values.size) {
        dataError(("210108211925", keys.size, values.size, keys.values.zipAll(values, null, null).joinln.sectionAllOff(2))) } // TODO

      keys.values.zip(values).pipe(obj)
    }

  // ---------------------------------------------------------------------------
  def untuplify1a(o: Obj, targetKey: Ren, splitter: StringSplitter, keys: Keyz): Obj =
      o .potch(targetKey.from)
        .pipe { case (targetOpt, restOpt) =>
          targetOpt
            .map {
              case seq: Seq[_] => seq.map(_untuplify1a(splitter, keys))
              case sgl         => sgl.pipe(_untuplify1a(splitter, keys)) }
            .map(update(restOpt, targetKey.to))
            .getOrElse(o) }

    // ---------------------------------------------------------------------------
    private def _untuplify1a(splitter: StringSplitter, keys: Keyz)(value: Any): Obj = {
      value
        .asInstanceOf[String]
        .pipe(splitter.apply)
        .pipe { values =>
          if (keys.size != values.size) {
            dataError("210108212537", keys.size, values.size, value, keys.values.zipAll(values, null, null).joinln.sectionAllOff(2))  }

          keys.values.zip(values).pipe(obj) } }

  // ---------------------------------------------------------------------------
  def untuplify1b(o: Obj, targetKey: Ren, arraySplitter: StringSplitter, /* entry */ splitter: StringSplitter, keys: Keyz): Obj =
      o .split(targetKey.from, arraySplitter.apply)
        .pipe(untuplify1a(_, targetKey, splitter, keys))

  // ---------------------------------------------------------------------------
  private def update(restOpt: Option[Obj], key: Key)(value: AnyValue): Obj =
    restOpt
      .map       { _.add(key,   value) }
      .getOrElse {   obj(key -> value) }

  // ===========================================================================
  def untuplify2z(targetKey: Ren)(entrySplitter: StringSplitter)(newKeys: Set[Key])(o: Obj): Obj =
      o .strings_ /* req */(targetKey.from)
        .map(_untuplify2z(entrySplitter))
        .map { o2 =>
          checkNewKeys(debug = o)(newKeys)(o2)

          o .replace(targetKey.from, o2)
            .rename(targetKey) }
        .getOrElse(o)

    // ---------------------------------------------------------------------------
    private def _untuplify2z(entrySplitter: StringSplitter)(values: Seq[String]): Obj =
      values
        .map { entrySplitter.apply(_) match {
            case Seq(key       ) => (key.symbol, "") // use empty string here since all is typed as string anyway
            case Seq(key, value) => (key.symbol, value) }}
        .pipe(obj)

  // ---------------------------------------------------------------------------
  def untuplify2a(targetKey: Ren)(entriesSplitter: StringSplitter, entrySplitter: StringSplitter)(newKeys: Set[Key])(o: Obj): Obj =
      o._transformRenx(targetKey) { value =>
        _untuplify2a(entriesSplitter, entrySplitter)(value)
        .tap(checkNewKeys(debug = o)(newKeys)) }

    // ---------------------------------------------------------------------------
    def _untuplify2a(entriesSplitter: StringSplitter, entrySplitter: StringSplitter)(value: AnyValue): Obj =
      value
          .asInstanceOf[String]
          .pipe(entriesSplitter.apply)
          .pipe(_untuplify2z(entrySplitter.apply))

  // ---------------------------------------------------------------------------
  def untuplify2b(targetKey: Ren)(arraySplitter: StringSplitter, entriesSplitter: StringSplitter, entrySplitter: StringSplitter)(newKeys: Set[Key])(o: Obj): Obj =
    o .split(targetKey.from, arraySplitter.apply)
      .pipe(untuplify2a(targetKey)(entriesSplitter, entrySplitter)(newKeys))

  // ===========================================================================
  private def checkNewKeys(debug: Obj)(newKeys: Set[Key])(o2: Obj) {
    if (o2.keySet.diff(newKeys).nonEmpty)
      dataError(s"TODO:NotNewKeys:210110142505:${o2.keys.#@@}:${o2.keySet.diff(newKeys).map(_.name).toSeq.sorted.#@@}:${newKeys.map(_.name).toSeq.sorted.#@@}:${debug.formatCompactJson}")
  }

}

// ===========================================================================

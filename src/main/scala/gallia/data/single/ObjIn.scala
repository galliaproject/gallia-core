package gallia.data.single

import aptus.{Anything_, Seq_}

// ===========================================================================
object ObjIn {

  private[gallia] def from(entries: Seq[(Key, AnyValue)]): Obj =  {
    if (!entries.isDistinct)
      throw new gallia.RuntimeError("210113195424 - DuplicateKeys: " + entries.map(_._1).duplicates.#@@) // TODO

    entries
      .toListMap
      .thn(Obj.build)
  }

  // ===========================================================================
  private[single] def normalize(data: UData): UData =
      data
        .flatMap { case (key, value) =>
          value match { // 201029155019
            case null | None | gallia.none | Seq() => None // see t210115144940

            case z: gallia.data.multiple.Objs => Some(key -> z.toListAndTrash) // TODO: keep? c210110112244

            case Some(x) => Some(key -> x)
            case x       => Some(key -> x) } }
}

// ===========================================================================

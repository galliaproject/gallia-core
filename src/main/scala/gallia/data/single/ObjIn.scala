package gallia
package data.single

// ===========================================================================
object ObjIn {

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

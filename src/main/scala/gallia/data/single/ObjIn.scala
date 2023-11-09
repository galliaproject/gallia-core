package gallia
package data
package single

// ===========================================================================
object ObjIn {

  def fromDataClassInstance[T <: Product : WTT](value: T): Obj =
    cls[T]
      .pipe(Instantiator2.valueToObj(_)(value))
      .asInstanceOf[Obj]

  // ===========================================================================
  private[single] def normalize(data: UData): UData = data.flatMap(normalizeEntry)

  // ---------------------------------------------------------------------------
  private[gallia] def normalizeEntry(entry: UEntry): Option[UEntry] =
    entry._2 match { // 201029155019
      case null | None | gallia.none | Seq() => None // see t210115144940

      case z: multiple.Objs => Some(entry._1 -> z.toListAndTrash) // TODO: keep? c210110112244

      case Some(x) => Some(entry._1 -> x)
      case x       => Some(entry._1 -> x) }

}

// ===========================================================================

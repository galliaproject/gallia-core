package gallia
package data
package single

// ===========================================================================
object ObjIn {

  def fromDataClassInstance[T <: Product : WTT](value: T): Obj = reflect.TypeNode.parse[T].leaf.forceDataClass.valueToObj(value).asInstanceOf[Obj]
  
  private[single] def normalize(data: UData): UData =
    data
      .flatMap { case (key, value) =>
        value match { // 201029155019
          case null | None | gallia.none | Seq() => None // see t210115144940

          case z: multiple.Objs => Some(key -> z.toListAndTrash) // TODO: keep? c210110112244

          case Some(x) => Some(key -> x)
          case x       => Some(key -> x) } }

}

// ===========================================================================

package gallia
package selection.untyped.processors

// ===========================================================================
private object UtsProcessorsUtils {

  def ifType[T: WTT](c: Cls): Seq[Key] =
    c.fields.flatMap { f =>
      if (f.ofni.isType[T]) Some(f.key)
      else                  None }

  // ---------------------------------------------------------------------------
  def ifTypeRecursively[T: WTT](path: OptionalKPath)(c: Cls): Seq[KPath] =
    c.fields.flatMap { f =>
      val path2 = path.appendLevel(f.key)

      f.nestedClassOpt match {
        case Some(c2) => ifTypeRecursively[T](path2)(c2)
        case None     =>
          if (f.ofni.isType[T]) Seq(path2.forcePath)
          else                  Nil } }

  // ---------------------------------------------------------------------------
  // outOfBounds(5)(Seq(0, 1, 2, 3, 4, 5, -1, -2, -3, -4, -5, -6)).p // List(5, -6)
  def outOfBounds(size: Int)(values: Seq[MIndex]): Seq[MIndex] =
    values
      .flatMap { mindex =>
        val index = unmirror(size)(mindex)

        if (index < 0 || index >= size) Some(mindex)
        else                            None }
      .distinct

  // ---------------------------------------------------------------------------
  /** eg -1 becomes 4 if there are 5 fields */
  /*private */def unmirror(size: Int)(value: MIndex): Index = if (value < 0) size + value else value

}

// ===========================================================================

package gallia
package io.out

// ===========================================================================
case class PrettyTableWritingContext(
        arraySeparator: String,
        hasHeader     : Boolean,
        nullValue     : String) {

    /* only to be used on small amount of data... */
    def formatTable(c: Cls)(z: gallia.Objs): Iterator[String] = {
      val _data: Seq[Seq[String]] =
        z .consumeSelfClosing
          .map(data.GalliaToTableData.convert(nullValue, arraySeparator)(c))
          .toList

      // ---------------------------------------------------------------------------
      val skeys = c.skeys

      // ---------------------------------------------------------------------------
      val transposed: Seq[Seq[String]] = 
        ((if (hasHeader) skeys else Nil) +:
            _data)
          .transpose

      // ---------------------------------------------------------------------------
      val maxes: Vector[aptus.Size] =
        transposed
          .map(_.map(_.size).max)
          .toVector

      // ---------------------------------------------------------------------------
      (  (if (hasHeader) Seq(
             skeys.zipWithIndex.map { case (k, i) => k .padTo(maxes(i), ' ') },
             skeys.zipWithIndex.map { case (_, i) => Seq.fill(maxes(i))("-").mkString })
            else Nil) ++
          _data.map(_.zipWithIndex.map { case (k, i) => k.padTo(maxes(i), ' ') }))
        .map(_.mkString(" | "))
        .iterator
    }

  }

  // ===========================================================================
  object PrettyTableWritingContext {
    val Default =
      PrettyTableWritingContext(
        arraySeparator = "|",
        hasHeader      = true,
        nullValue      = "")
  }

// ===========================================================================

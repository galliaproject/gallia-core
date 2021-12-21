package gallia
package io.out

// ===========================================================================
case class PrettyTableWritingContext(
        arraySeparator: String,
        hasHeader     : Boolean,
        nullValue     : String) {

	  /* only to be used on small amount of data... */  
    def formatTable(keys: Seq[String])(data: gallia.Objs): Iterator[String] = {      
      val _data: Seq[Seq[String]] = data.toListAndTrash.map(formatRowValues(keys))

      val transposed: Seq[Seq[String]] = 
        ((if (hasHeader) keys else Nil) +:
            _data)
          .transpose

      val maxes: Vector[aptus.Size] = transposed.map(_.map(_.size).max).toVector      
      
      // ---------------------------------------------------------------------------
      (  (if (hasHeader) Seq(
             keys.zipWithIndex.map(formatPaddedValue(maxes)), 
             keys.zipWithIndex.map { case (_, i) => Seq.fill(maxes(i))("-").mkString })
            else Nil) ++
          _data.map(_.zipWithIndex.map(formatPaddedValue(maxes))))
        .map(_.mkString(" | "))
        .iterator ++ 
        Iterator("") // trailing newline        
    }

    // ===========================================================================
    private def formatRowValues(keys: Seq[String])(o: gallia.Obj): Seq[String] =
      keys
        .map { key =>    
          o .opt(key)
            .map(TableWritingContext.formatValue(arraySeparator))
            .getOrElse(nullValue) }

    // ---------------------------------------------------------------------------
    private def formatPaddedValue(maxes: Vector[Int])(pair: (String /* value */, aptus.Index)): String =
      maxes(pair._2).pipe { max => pair._1.padTo(max, ' ') }
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
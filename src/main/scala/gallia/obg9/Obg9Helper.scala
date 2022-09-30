package gallia
package atoms
package obg9

// ===========================================================================
trait Obg9Helper { self: Obg9 =>

  @inline protected def _reorderKeys(procedure: (Ori, Dest) => Unit): Array[Any] = {
      val newData = _cloneArray()
      procedure(data, newData)

      newData
    }

    // ---------------------------------------------------------------------------
    @inline protected def _transform(target: Index)(f: Any => Any): Array[Any] = {
      val newData = _cloneArray()
      newData(target) = f(data(target))

      newData
    }

    // ---------------------------------------------------------------------------
    @inline protected def _setValue(target: Index)(newValue: Any): Array[Any] = {
      val newData = _cloneArray()
      newData(target) = newValue

      newData
    }

  // ---------------------------------------------------------------------------
  @inline protected def _putLast(newValue: Any): Array[Any] = {
    val newData = _cloneArray(size + 1, size)    
    newData(size) = newValue

    newData    
  }  

  // ---------------------------------------------------------------------------
  @inline protected def _putLast(extra: Size, values: Iterable[Any]): Array[Any] = {
    val newData = _cloneArray(size + extra, size)    
    copyLast(values)(newData)

    newData    
  }  

  // ---------------------------------------------------------------------------
  @inline protected def _retainOrRemove(newSize: Size)(procedure: (Ori, Dest) => Unit): Array[Any] = {
    val newData = new Array[Any](newSize)
    procedure(data, newData)

    newData    
  }      

  // ===========================================================================
  protected def copyLast(values: Iterable[Any])(dest: Dest): Unit = {
    var j = size
    values.foreach { value =>
      dest(j) = value
      j += 1 }       
  }
  
  // ---------------------------------------------------------------------------
  protected def copyRetain(indices: Set[Index])(ori: Ori, dest: Dest): Unit = {
      var j = 0
      for (i <- 0 until size) {
        if (indices.contains(i)) {
          dest(j) = ori(i)
          j += 1 } }
    }
  
    // ---------------------------------------------------------------------------
    protected def copyRemove(indices: Set[Index])(ori: Ori, dest: Dest): Unit = {
      var j = 0
      for (i <- 0 until size) {
        if (!indices.contains(i)) {
          dest(j) = ori(i)
          j += 1 } }
    }
    
    // ---------------------------------------------------------------------------
    protected def copyReorderNonRecursively(reorderedIndices: Seq[Index])(ori: Ori, dest: Dest): Unit = {  
      var j = 0
      for (i <- reorderedIndices) {
        dest(j) = ori(i)
        j += 1 }
    }

    // ---------------------------------------------------------------------------
    protected def copyReorderRecursively(reordering: RecursiveReordering)(ori: Ori, dest: Dest): Unit = {    
      var j = 0
      for (r <- reordering.values) {        
        val value =
          r.nesting match {
            case None         =>         ori(r.index)
            case Some(nested) => nesting(ori(r.index))(nested.optional, nested.multiple) { _.reorderKeysRecursively(nested.reordering) } }                        

        dest(j) = value
        j += 1 }   
    }

  // ===========================================================================
  protected def copyRetainWithNesting(ctx: RemoveOrRetainWithNestingCtx)(ori: Ori, dest: Dest): Unit = {
    copyRemoveOrRetainWithNesting(ctx)(identity, _ retainWithNesting9 _)(ori, dest) }
  
  // ---------------------------------------------------------------------------
  protected def copyRemoveWithNesting(ctx: RemoveOrRetainWithNestingCtx)(ori: Ori, dest: Dest): Unit = {
    copyRemoveOrRetainWithNesting(ctx)(!_, _ removeWithNesting9 _)(ori, dest) }
  
  // ---------------------------------------------------------------------------
  private def copyRemoveOrRetainWithNesting
      (ctx: RemoveOrRetainWithNestingCtx)
      (inclusion: Boolean => Boolean,
       recursion: (Obg9, RemoveOrRetainWithNestingCtx) => Obg9)
      (ori: Ori, dest: Dest): Unit = {
    val topLevelIndices = ctx.topLevelIndices
    
    var j = 0
    for (i <- _indices) {      
      val originalValue = ori(i)

      ctx
        .nesting
        .get(i)
         match {
          case Some(bar) =>                      
            dest(j) = nesting(originalValue)(bar.optional, bar.multiple) { recursion(_, bar.ctx) }
            j += 1
    
          case None =>
            if (inclusion(topLevelIndices.contains(i))) {
              dest(j) = originalValue
              j += 1 }            
      }
    }  
  }  

  // ===========================================================================
  /*private */def _cloneArray(): Array[Any] = _cloneArray(size, size)

  // ---------------------------------------------------------------------------
  /*private */def _cloneArray(size1: Int, size2: Int): Array[Any] = {
    val newData: Array[Any] = new Array[Any](size1)    
    java.lang.System.arraycopy(data, 0, newData, 0, size2)
  
    newData
  }
  
  // ===========================================================================
  protected def drillToValue(indices: PPairs): Any = {    
    val first = indices.head

    indices.tailOpt match {
      case None       => data.value(first)        
      case Some(more) => data.value(first).pipe(nesting(_)(first.optional, first.multiple) { _.drillToValue(more) }) }    
  }
  
  // ---------------------------------------------------------------------------
  protected def nesting[T](value: Any)(optional: Optional, multiple: Multiple)(f: Obg9 => T): Any =
    (optional, multiple) match {
      case (false, false) => value.asInstanceOf[           Obg9  ]      .pipe(f)            
      case (true,  false) => value.asInstanceOf[Option[    Obg9 ]].map(       f ) .orNull
      case (false, true ) => value.asInstanceOf[       Seq[Obg9] ]      .map (f )
      case (true , true ) => value.asInstanceOf[Option[Seq[Obg9]]].map(_.map (f)).orNull }


  // ===========================================================================
  @inline protected def _indices: Range = 0 until size
  
  // ---------------------------------------------------------------------------
  @inline protected def optionToNull(value: Any): Any = value.asInstanceOf[Option[_]].orNull
  
  // ---------------------------------------------------------------------------
  @inline protected def optionToNull2(value: Any) = value match {    
    case None    => null
    case Some(x) => x
    case      x  => x }
  
}

// ===========================================================================

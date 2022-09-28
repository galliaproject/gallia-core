package gallia
package atoms
package obg9

import gallia.domain.KPaths2

// ===========================================================================
object Obg9Utils { import Obg9Creators._	
  def hack = obg9.hack

  // ===========================================================================
  def rename(c1: Cls, c2: Cls): _Generic9 = generic(c1, c2)(identity)

	// ---------------------------------------------------------------------------
	def reorderKey(c1: Cls, c2: Cls)(recursively: Boolean, f: Seq[SKey] => Seq[SKey]): _Generic9 =
      if (recursively)    RecursiveReorderingCreator(c1)(f).pipe { reordering => generic(c1, c2) { _.reorderKeysRecursively   (reordering) } }
      else             NonRecursiveReorderingCreator(c1)(f).pipe { reordering => generic(c1, c2) { _.reorderKeysNonRecursively(reordering) } }

  // ---------------------------------------------------------------------------
  def replace(c1: Cls, c2: Cls)(key: Key, value: Any): _Generic9 = generic(c1, c2)(_.replace(c1.keyIndex(key), value))// TODO: t220413182403 - combo version too?

  // ===========================================================================
  def add(c1: Cls, c2: Cls)(entries: domain.KVEs): _Generic9 =
    entries
      .forceDataEntries
      .map(_._2)
      .pipe { newValues =>
        if (newValues.size == 1) generic(c1, c2)(_.putLast(newValues.head)) // TODO: t220414101122 - up to 5?
        else                     generic(c1, c2)(_.putLast(newValues, newValues.size)) }

  // ===========================================================================
  def retain(c1: Cls, c2: Cls)(pathz: KPathz): _Generic9 = _retain(c1)(pathz).pipe(generic(c1, c2))
  def remove(c1: Cls, c2: Cls)(pathz: KPathz): _Generic9 = _remove(c1)(pathz).pipe(generic(c1, c2))

    // ---------------------------------------------------------------------------
    private def _retain(c1: Cls)(pathz: KPathz): Obg9 => Obg9 =
      pathz.keyzOpt match {
        case None =>
          val ctx = RemoveOrRetainWithNestingCtxCreator(c1, pathz.distinct)
          _.retainWithNesting9(ctx)

        // ---------------------------------------------------------------------------
        case Some(keyz) =>
          val indices = c1.indices(keyz)

          Obg9Creators.tryRetainContiguous(
              totalSize = c1.size,
              indices   = indices)
            match {
              case Some(ctx) => _.retainContiguous(ctx)
              case None      => _.retainArbitrary(indices.toSet, indices.size) } } // TODO: ctx

    // ---------------------------------------------------------------------------
    private def _remove(c1: Cls)(pathz: KPathz): Obg9 => Obg9 =
      pathz.keyzOpt match {
        case None =>
          val ctx = RemoveOrRetainWithNestingCtxCreator(c1, pathz.distinct)
          _.removeWithNesting9(ctx)

        // ---------------------------------------------------------------------------
        case Some(keyz) =>
          val indices = c1.indices(keyz)

          Obg9Creators.tryRemoveContiguous(
              totalSize = c1.size,
              indices   = indices)
            match {
              case Some(ctx) => _.removeContiguous(ctx)
              case None      => _.removeArbitrary(indices.toSet, c1.size - indices.size) } }

  // ===========================================================================
  def setDefaultValue(c1: Cls, c2: Cls)(value: Any)(path: KPath): _Generic9 =
    path.leafOpt match {
      case Some(leaf) =>
        val target = c1.keyIndex(leaf)
        generic(c1, c2)(_.setDefaultValueLeafOR(target, value))
      case None => c1.pathIndices(path).pipe { indices => generic(c1, c2)(_.setDefaultValueNestingOR(indices, value)) } }

  // ---------------------------------------------------------------------------
  def removeConditionally(c1: Cls, c2: Cls)(pred: Any => Boolean)(path: KPath): _Generic9 =
    path.leafOpt match {
      case Some(leaf) =>
        val target = c1.keyIndex(leaf)
        if (c1.isOptional(leaf)) generic(c1, c2)(_.removeConditionallyOX(target, pred))
        else                     generic(c1, c2)(_.removeConditionallyRX(target, pred))
      case None => c1.pathIndices(path).pipe { indices => generic(c1, c2)(_.removeConditionallyOX2(indices, pred)) }}

	// ===========================================================================
  def transform(c1: Cls, c2: Cls, optional2: Boolean)(f: _ff11)(values: Seq[KPath]): Seq[_Generic9] =
    values
      .map { path =>
        path
          .leafOpt
          .map { leaf =>
            val index     = c1.keyIndex  (leaf)
            val optional1 = c1.isOptional(leaf)
            (optional1, optional2)  match {
              case (false, false) => generic(c1, c2)(_.transformRR(index, f))
              case (false, true ) => generic(c1, c2)(_.transformRO(index, f))
              case (true , false) => generic(c1, c2)(_.transformOR(index, f))
              case (true , true ) => generic(c1, c2)(_.transformOO(index, f)) } }
          .getOrElse {
            c1.pathIndices(path)
              .pipe { keyPath =>
                generic(c1, c2)(_.transformNested(keyPath, f)) }}}

  // ===========================================================================
  def generate(c1: Cls, c2: Cls)(path: KPath, optionalOutput: Boolean)(f: _ff11): _Generic9 =
    c1.either(path) match {

      case Left (index) =>
        (c1.isOptional(path), optionalOutput) match {
          case (false, false) => generic(c1, c2)(_.generateWithoutNestingAsLastRR(index, f))
          case (false, true)  => generic(c1, c2)(_.generateWithoutNestingAsLastRO(index, f))

          case (true,  false) => generic(c1, c2)(_.generateWithoutNestingAsLastOR(index, f))
          case (true,  true)  => generic(c1, c2)(_.generateWithoutNestingAsLastOO(index, f)) }

      // ---------------------------------------------------------------------------
      case Right(drilled) =>
        // input optionality is handled by the drilling process
        if (optionalOutput) generic(c1, c2)(_.generateWithNestingAsLastXO(drilled, f))
        else                generic(c1, c2)(_.generateWithNestingAsLastXR(drilled, f))
    }

    // ---------------------------------------------------------------------------
    def generate2to1(c1: Cls, c2: Cls)(path: KPaths2)(f: _ff21): _Generic9 = {
      val (target1, target2) = c1.pathIndicess(path)
      generic(c1, c2)(_.generate2to1(target1, target2, f))
    }

    // ---------------------------------------------------------------------------
    def generate1to2(c1: Cls, c2: Cls)(path: KPath)(f: _ff12): _Generic9 = {
      val target = c1.pathIndices(path)
      generic(c1, c2)(_.generate1to2(target, f))
    }

    // ---------------------------------------------------------------------------
    def fuse2to1(c1: Cls, c2: Cls)(path: KPaths2)(f: _ff21): _Generic9 = {
      val (target1, target2) = c1.pathIndicess(path)

      // TODO: t220418154807: support paths for destination
      if (false) { // t220419103640 - fusion/fission should use the earliest top level field for location (else add at the end)
        val outputIndex = _outputIndex(c1)(path.paths)
        generic(c1, c2)(_.fuse2to1(target1, target2, outputIndex, f))
      } else {
        val removal: Obg9 => Obg9 =
          c1
            .add (_tmp.boolean) /* temporary dummy field */
            .pipe(_remove(_)(path.pathz))

        generic(c1, c2)(_.fuse2to1(target1, target2, removal, f))
      }
    }

    // ---------------------------------------------------------------------------
    private def _outputIndex(c: Cls)(paths: Seq[KPath]): Index =
      paths
        .find     (_.isLeaf)
        .map      (_.forceLeaf)
        .map      (c.keyIndex)
        .getOrElse(c.size)

  // ---------------------------------------------------------------------------
  def fission1to2(c1: Cls, c2: Cls)(path: KPath)(f: _ff12): _Generic9 = {
    val target = c1.pathIndices(path)

    // TODO: t220418154807: support paths for destination
    if (false) { // t220419103641 - fission should use the earliest field for FIRST location, at least the first non path input (else use last); rest go at the end (shifting all is costly)
      val outputIndex = _outputIndex(c1)(Seq(path))
      generic(c1, c2)(_.fission2to1(target, outputIndex, f))
    } else {
      val removal: Obg9 => Obg9 =
        c1
          .add (_tmp1.boolean) /* temporary dummy field */
          .add (_tmp2.boolean) /* temporary dummy field */
          .pipe(_remove(_)(KPathz(Seq(path))))

      generic(c1, c2)(_.fission2to1(target, removal, f))
    }
  }

}

// ===========================================================================
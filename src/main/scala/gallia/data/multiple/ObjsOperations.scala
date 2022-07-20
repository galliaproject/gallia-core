package gallia
package data
package multiple

// ===========================================================================
trait ObjsOperations { self: Objs =>
  // note:
  // - t210104164036 and t210104164037: renaming(s) will only be applicable to the standalone version
  // - t210104164036 and t210104164037: more generally, most of this will be rewritten in that context...

  private implicit def _to(x: List[Obj]): Objs = Objs.from(x)

  // ===========================================================================
  def transformPath            (target: KPathW,   f: AnyValue => AnyValue): Objs = map(_.transformPath    (target, f)) // TODO: phase out
  def transformPathx           (target: KPathW,   f: AnyValue => AnyValue): Objs = map(_.transformPathx   (target, f))
  def transformPathPair        (target: PathPair, f: AnyValue => AnyValue): Objs = map(_.transformPathPair(target, f))
  def transformWhateverPathPair(target: PathPair, f: AnyValue => AnyValue, checkType: Boolean): Objs = map(_.transformWhateverPathPair(target, f, checkType)) // only for Whatever to Whatever transformation...

  // ---------------------------------------------------------------------------
  def _transformKey            (key: Key,                                        f: AnyValue => AnyValue): Objs = map(_.transformKey(key, f))// TODO: phase out
  def _transformKeyx           (key: Key,                                        f: AnyValue => AnyValue): Objs = map(_.transformKeyx(key, f))
  def _transformRenx           (key: Ren)                                       (f: AnyValue => AnyValue): Objs = map(_.transformRenx(key)(f))
  def _transformKeyPair        (key: Key, optional: Boolean)                    (f: AnyValue => AnyValue): Objs = map(_.transformKeyPair(key, optional)(f))
  def _transformWhateverKeyPair(key: Key, optional: Boolean, checkType: Boolean)(f: AnyValue => AnyValue): Objs = map(_.transformWhateverKeyPair(key, optional, checkType)(f)) // abstracts requiredness + optionally check resulting type
        
  // ===========================================================================
  def union(that: Objs): Objs = self.values.union(that.values)           .pipe(Objs.build)
  def zip  (that: Objs): Objs = self.values.zip  (that.values, _ merge _).pipe(Objs.build)

  // ===========================================================================
  def ensureDistinct(c: Cls): Either[(Int, Int), Objs] = {
      val originalSize  = size
      val distinctified = distinct(c)
      val newSize       = distinctified.size
      if (newSize < originalSize) Left(originalSize, newSize)
      else                        Right(distinctified) }

    // ---------------------------------------------------------------------------
    def ensureDistinctBy(c: Cls)(keys: Keyz): Either[(Int, Int), Objs] =
      self
        .fork(keys)
        .ensureDistinct(c)
        .map(_ => self)

      // ===========================================================================
      private[multiple] def fork(keys: Keyz): Objs =
        self
          .map { o =>
            o.putKey(
                _tmp,
                o.retainOpt(keys).isDefined)
          .retain(keys) } // guaranteed always at least _tmp
}

// ===========================================================================

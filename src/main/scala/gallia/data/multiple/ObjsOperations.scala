package gallia.data.multiple

import scala.reflect.classTag
import aptus.Anything_

import gallia._
import gallia.domain.SortingPair
import gallia.data.single.ObjOrdering

// ===========================================================================
trait ObjsOperations { self: Objs =>
  // note:
  // - t210104164036 and t210104164037: renaming(s) will only be applicable to the standalone version
  // - t210104164036 and t210104164037: more generally, most of this will be rewritten in that context...

  private implicit def _to(x: Seq[Obj]): Objs = Objs.from(x)

  // ===========================================================================
  def toUpperCase(key: Key): Objs = map(_.toUpperCase(key))
  //TODO: more... (see t210104164037)

  // ===========================================================================
  def distinct: Objs = self._modifyUnderlyingStreamer(_.distinct)

  // ---------------------------------------------------------------------------
  def union(that: Objs): Objs = self.values.union(that.values).pipe(Objs.build)

  // ---------------------------------------------------------------------------
  def sortBy(c: Cls, pair: SortingPair): Objs =
    _modifyUnderlyingStreamer {
      _.sortBy(
            classTag[Option[Obj]],
            ObjOrdering.optionObjOrdering(c, pair)) {
        _.retainOpt(c.keyz) } }

  // ===========================================================================
  def ensureDistinct: Either[(Int, Int), Objs] = {
      val originalSize  = size
      val distinctified = distinct
      val newSize       = distinctified.size
      if (newSize < originalSize) Left(originalSize, newSize)
      else                        Right(distinctified)
    }

    // ---------------------------------------------------------------------------
    def ensureDistinctBy(keys: Keyz): Either[(Int, Int), Objs] =
      self
        .fork(keys)
        .ensureDistinct
        .map(_ => self)

      // ===========================================================================
      private[multiple] def fork(keys: Keyz): Objs =
        self
          .map { o =>
            o.put(
                gallia._tmp,
                o.retainOpt(keys).isDefined)
          .retain(keys) } // guaranteed always at least _tmp
}

// ===========================================================================

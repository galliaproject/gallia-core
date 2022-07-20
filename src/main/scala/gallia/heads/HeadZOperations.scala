package gallia
package heads

import actions.ActionsZZ._
import actions.ActionsOthers._

// ===========================================================================
trait HeadZOperations { self: HeadZ =>

  def distinct: Self = zz(Distinct())

  @Distributivity def distinctByAdjacency: Self = ??? //TODO: t210117113705 - convenient if mostly grouped already; for distributivity, do at least per partition

  // ===========================================================================
  def force = new { // TODO: t201016121417 - or as convert... [term:x]
      def one: HeadU = zu(ForceOne)

      // ---------------------------------------------------------------------------
      /** very costly if not checkpointed first (unless in-memory) ... */
      def distinct: HeadZ = _ensureUniqueness(None)
    }

    // ---------------------------------------------------------------------------
    // TODO: t201016122408 - or "assertUniqueness"? [term:x]
    /** can be very costly ... */
    def ensureUniqueness: Self = force.distinct // for good measure

      def ensureUniquenessById                                   : Self =  ensureUniquenessBy(_id)
      def ensureUniquenessBy(key : KeyW)                         : Self =  ensureUniquenessBy(Keyz.from(key))
      def ensureUniquenessBy(key1: KeyW, key2: KeyW, more: KeyW*): Self =  ensureUniquenessBy(Keyz.from(key1, key2, more))
      def ensureUniquenessBy(keys: KeyWz)                        : Self = _ensureUniqueness(Some(keys.keyz))

      // ---------------------------------------------------------------------------
      private def _ensureUniqueness(keyzOpt: Option[Keyz]) =
        mergeObjsVle(isDistinctHead(keyzOpt)) { (z, distinct) =>
          if (!distinct) atoms.AtomsZZ._EnsureUniquenessBy.error(0, 0)(z) else z } // TODO: provide sizes instead of just boolean

      // ---------------------------------------------------------------------------
      private def isDistinctHead(keyzOpt: Option[Keyz]): HeadV[Boolean] = {
        val tmp = keyzOpt
          .map(_.rpathz)
          .map      (self.retain(_))
          .getOrElse(self)

        val         sizeHead: HeadV[Int] = tmp.distinct.size
        val distinctSizeHead: HeadV[Int] = tmp         .size

        sizeHead.combine(distinctSizeHead).using(_ - _ == 0) }

  // ===========================================================================
  // TODO:
  // - t210110094731 - selection
  // - t210131102306 - cascade if multiple?
  def flattenBy(target: RPathW): HeadZ = rename(target.value).zz(FlattenByZ(target.value.to))

    def flattenByBoth : HeadZ = flattenByLeft.flattenByRight
    def flattenByLeft : HeadZ = flattenBy(_left)
    def flattenByRight: HeadZ = flattenBy(_right)
    def flattenByGroup: HeadZ = flattenBy(_group)

    // ---------------------------------------------------------------------------
    // TODO: t210131102354 - common enough?
    //def flattenAndUnnestByBoth : Self = flattenAndUnnestByLeft.flattenAndUnnestByRight
    //def flattenAndUnnestByGroup: Self = flattenBy(reserved._group).map(_.unnestObject(reserved._group))
    //def flattenAndUnnestByLeft : Self = flattenBy(reserved._group).map(_.unnestObject(reserved._left ))
    //def flattenAndUnnestByRight: Self = flattenBy(reserved._group).map(_.unnestObject(reserved._right))

  // ===========================================================================
  // zu

  def mergeAll: HeadU = zu(MergeAll)

  // ---------------------------------------------------------------------------
  // TODO: t210124100009 - better names [term:x]...
  def asArray1               : HeadU = zu(AsArray1)
  def asArray1(target: ActualRPathW): HeadU = rename(target.value).asArray1

  def asArray2              : HeadU = asArray2(_array)
  def asArray2(target: KeyW): HeadU = zu(AsArray2(target.value))

  // ---------------------------------------------------------------------------
//def unnarrayUsing = TODO - see t210131104517

  // ===========================================================================
  // TODO: t210131110455
    //def   tabularize: Self = self // TODO: t210131110456
    //def untabularize: Self = self // TODO: t210131110457
      //@Scalability def        cartesianProduct(k1, k2, more) = // see 210131105936@w (ObjUFlattening)    ; see 210131110106@w (aptus.CartesianProduct PoC)
      //@Scalability def reverseCartesianProduct(k1, k2, more) = // see 210131105955@w (ObjUTableQuickTest); see 210131105401@w (aptus.ReverseCartesianProduct PoC)
}

// ===========================================================================

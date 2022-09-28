package gallia
package atoms
package obg9

import aptus._
import Obg9Contexts._

// ===========================================================================
/** obg9 is codename for memory-optimized Obj counterpart (not in use yet) */
class Obg9( // good for dense data
      val size: Int,
      val data: Array[Any])
    extends Obg9Helper {
  private implicit def _fromArray(x: Array[Any]): Obg9 = new Obg9(size, x)

  // ---------------------------------------------------------------------------
  def transformRR(target: Index, f: _ff11): Obg9 = _setValue(target) { f(       data(target))  }
  def transformRO(target: Index, f: _ff11): Obg9 = _setValue(target) { f(       data(target)) .pipe(optionToNull) }
  def transformOR(target: Index, f: _ff11): Obg9 = _setValue(target) { f(Option(data(target))) }
  def transformOO(target: Index, f: _ff11): Obg9 = _setValue(target) { f(Option(data(target))).pipe(optionToNull) }

  // ---------------------------------------------------------------------------
  def transformNested(indices: PPairs, f: _ff11): Obg9 = { // TODO: t220418141410 - more specialized one (eg for 1 level of nesting)
    val first = indices.head
    
    _setValue(first.index) {
      val value = data.value(first)

      indices.tailOpt match {
        case None       => f      (value)        
        case Some(more) => nesting(value)(first.optional, first.multiple) {
          _.transformNested(indices = more, f) } } } }

  // ===========================================================================
  // rename: no op

  // ---------------------------------------------------------------------------
  def replace(target: Index, value: Any): Obg9 = _setValue(target) { value } // TODO: t220413182403 - combo versions too

  // ---------------------------------------------------------------------------
  def putLast(value :          Any)             : Obg9 = _putLast(       value)
  def putLast(values: Iterable[Any], extra: Int): Obg9 = _putLast(extra, values) // TODO: worth hardcoding up to 5 dedicated ones?

  // ===========================================================================
  def retainContiguous(ctx: RetainContiguousCtx): Obg9 = _retainOrRemove(ctx.newSize) { ctx.singleCopy }
  def removeContiguous(ctx: RemoveContiguousCtx): Obg9 = _retainOrRemove(ctx.newSize) { ctx.doubleCopy }

  // ---------------------------------------------------------------------------
  def retainArbitrary(indices: Set[Index], newSize: Size): Obg9 = _retainOrRemove(newSize) { copyRetain(indices) } // assumes newSize not to be zero (should be validated by this point)
  def removeArbitrary(indices: Set[Index], newSize: Size): Obg9 = _retainOrRemove(newSize) { copyRemove(indices) } // assumes newSize not to be zero (should be validated by this point)

  // ---------------------------------------------------------------------------
  def retainWithNesting9(ctx: RemoveOrRetainWithNestingCtx): Obg9 = _retainOrRemove(ctx.newSize) { copyRetainWithNesting(ctx) }
  def removeWithNesting9(ctx: RemoveOrRetainWithNestingCtx): Obg9 = _retainOrRemove(ctx.newSize) { copyRemoveWithNesting(ctx) }

  // ===========================================================================
  def reorderKeysNonRecursively(reordering: NonRecursiveReorderingCtx): Obg9 = _reorderKeys { copyReorderNonRecursively(reordering) } // TODO: t220414091459 - specialized versions for eg reorderAsFirst/Last at least
  def reorderKeysRecursively   (reordering: RecursiveReordering)      : Obg9 = _reorderKeys { copyReorderRecursively   (reordering) }
    
  // ---------------------------------------------------------------------------
  def generateWithoutNestingAsLastRR(target: Index, f: _ff11): Obg9 = _putLast { f(data(target)) }
  def generateWithoutNestingAsLastRO(target: Index, f: _ff11): Obg9 = _putLast { f(data(target)).pipe(optionToNull) }
  def generateWithoutNestingAsLastOR(target: Index, f: _ff11): Obg9 = _putLast { f(Option(data(target))) }
  def generateWithoutNestingAsLastOO(target: Index, f: _ff11): Obg9 = _putLast { f(Option(data(target))).pipe(optionToNull) }

  // ---------------------------------------------------------------------------
  def generateWithNestingAsLastXR(target: PPairs, f: _ff11): Obg9 = _putLast { f(drillToValue(target)) }
  def generateWithNestingAsLastXO(target: PPairs, f: _ff11): Obg9 = _putLast { f(drillToValue(target)).pipe(optionToNull) }  
  
  // ---------------------------------------------------------------------------
  def generate2to1(target1: PPairs, target2: PPairs, f: _ff21): Obg9 = _putLast    { f(drillToValue(target1), drillToValue(target2)).pipe(optionToNull2) }
  def generate1to2(target : PPairs,                  f: _ff12): Obg9 = _putLast(2, { f(drillToValue(target)).toSeq                  .map (optionToNull2) })

  // ---------------------------------------------------------------------------
  // t220419103640 - fusion should use the earliest field for location, at least the first non path input (else use last)
  def fuse2to1(target1: PPairs, target2: PPairs, output: Index         , f: _ff21): Obg9 =          _setValue(output) { f(drillToValue(target1), drillToValue(target2)).pipe(optionToNull2) }
  def fuse2to1(target1: PPairs, target2: PPairs, removals: Obg9 => Obg9, f: _ff21): Obg9 = removals(_putLast          { f(drillToValue(target1), drillToValue(target2)).pipe(optionToNull2) })

  // ---------------------------------------------------------------------------
  // t220419103641 - fission should use the earliest field for FIRST location, at least the first non path input (else use last); rest go at the end (shifting all is costly)
  def fission2to1(target: PPairs, output: Index         , f: _ff12): Obg9 = ???//    _setValue(output)     { f(drillToValue(target))      .pipe(optionToNull2) }
  def fission2to1(target: PPairs, removals: Obg9 => Obg9, f: _ff12): Obg9 = removals(_putLast          (2, { f(drillToValue(target)).toSeq.map (optionToNull2) }))

  // ---------------------------------------------------------------------------
  def setDefaultValueLeafOR   (target: Index,  defaultValue: Any): Obg9 = _transform(target) { oldValue => if (oldValue == null) defaultValue else oldValue }
  def setDefaultValueNestingOR(target: PPairs, defaultValue: Any): Obg9 = transformNested(target, (oldValue: Any) => oldValue.asInstanceOf[Option[_]].getOrElse(defaultValue))

  // ---------------------------------------------------------------------------
  def removeConditionallyRX(target: Index, pred: Any => Boolean): Obg9 = _transform(target) { oldValue => if (  pred(oldValue))              null else oldValue }
  def removeConditionallyOX(target: Index, pred: Any => Boolean): Obg9 = _transform(target) { oldValue => if (Option(oldValue).forall(pred)) null else oldValue }

  def removeConditionallyOX2(target: PPairs, pred: Any => Boolean): Obg9 = transformNested(target, oldValue => if (Option(oldValue).forall(pred)) null else oldValue)
}

// ===========================================================================


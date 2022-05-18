package gallia
package heads
package common

import actions.ActionsUUTransformObjectIf.{TransformObjectIf, TransformObjectIfW}

// ===========================================================================
trait HeadCommonTransformObjectIf[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>
  import gallia.FunctionWrappers._

  private type TransformSomeObjects[T] = TSL.TransformSomeObjects.TSelector[T]
  import   TSL.TransformSomeObjects._

  // ===========================================================================
  class _TransformSomeObjects private[heads] (field: RPathW) {
      def matching        (target: (SKey, AnyValue))                           : __TransformSomeObjectsW   = new __TransformSomeObjectsW(field, KPath.from(target._1), target._2)
      def matching        (target: KPathW,                  value: AnyValue)   : __TransformSomeObjectsW   = new __TransformSomeObjectsW(field, target, value)

      def matching[O: WTT](target: TransformSomeObjects[O], value: O)          : __TransformSomeObjects[O] = new __TransformSomeObjects (field, target, _ == value)
      def matching[O: WTT](target: TransformSomeObjects[O], pred: O => Boolean): __TransformSomeObjects[O] = new __TransformSomeObjects (field, target, pred)  }

    // ---------------------------------------------------------------------------
    class __TransformSomeObjects[O: WTT] private[heads] (field: RPathW, target: TransformSomeObjects[O], pred: O => Boolean) {
      def using(f: HeadU => HeadU): Self2 =
        transformAllObjects(field).using {
          _.transformObjectIf(target).matches(pred).using(f) } }

    // ---------------------------------------------------------------------------
    class __TransformSomeObjectsW private[heads] (field: RPathW, target: KPathW, value: AnyValue) {
      def using(f: HeadU => HeadU): Self2 =
        transformAllObjects(field).using {
          _.transformObjectIf(target.value).hasValue(value).using(f) } }

  // ===========================================================================
  def transformObjectIf        (target: KPath)                  : _TransformObjectIfW   = new _TransformObjectIfW  (target)
  def transformObjectIf[O: WTT](target: TransformSomeObjects[O]): _TransformObjectIf[O] = new _TransformObjectIf[O](target)

    // ===========================================================================
    class _TransformObjectIf[O: WTT] private[heads] (target: TransformSomeObjects[O]) {
        def hasValue(value: O)       : __TransformObjectIf[O] = matches(_ == value)
        def matches (f: O => Boolean): __TransformObjectIf[O] = new __TransformObjectIf(target, f) }

      // ---------------------------------------------------------------------------
      class _TransformObjectIfW private[heads] (target: KPath) {
        def hasValue(value: AnyValue)       : __TransformObjectIfW = matches(_ == value)
        def matches (f: AnyValue => Boolean): __TransformObjectIfW = new __TransformObjectIfW(target, f) }

      // ===========================================================================
      class __TransformObjectIf[O: WTT] private[heads] (target: TransformSomeObjects[O], p: O => Boolean) {
          def using(f: HeadO => HeadO): Self2 = self2 :+
            TransformObjectIf(resolve(target), pwrap(p), f) }

        // ---------------------------------------------------------------------------
        class __TransformObjectIfW private[heads] (target: KPath, p: AnyValue => Boolean) {
          def using(f: HeadO => HeadO): Self2 = self2 :+
            TransformObjectIfW(target, pwrap(p), f) }
}

// ===========================================================================

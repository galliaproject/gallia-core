package gallia
package heads
package common

import target.TypeDuo
import FunctionWrappers._
import actions.common.ActionsCommonDataClasses._

// ===========================================================================
trait HeadCommonDataClasses[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // 220412171654

  // 220914145147 - transformDataClass
  // TODO: t220923095400 - in the future .transform(_.dataClass[Foo]("foo"))
  def transformDataClass[O: WTT](target: KeyW) = new _TransformDataClass(target) // TODO: better name?

    // ---------------------------------------------------------------------------
    class _TransformDataClass[O: WTT] private[HeadCommonDataClasses] (target: KeyW) {
      def using[D: WTT](f: O => D): Self2 = self2 :+
        TransformDataClass(target.value, from = TypeDuo.build[O], to = TypeDuo.build[D], wrap(f)) }

  // ===========================================================================
  /** does not *have* to be co-transform, can also only use one field (convenient for encapsulation) */
  // TODO: t220913144308 - macro versions
  def cotransformViaDataClass[O: WTT] = new _CotransformViaDataClass[O](TypeDuo.build[O]) // TODO: better name? coin term?

    // ---------------------------------------------------------------------------
    class _CotransformViaDataClass[O: WTT] private[HeadCommonDataClasses] (origin: TypeDuo) {
      //TODO: t220801133413 - any way to forbid Option or Seq? at least as validation?

      // ---------------------------------------------------------------------------
      def usingWithErasing[D <: DataClass: WTT](f: O => D): Self2 = self2 :+
        CotransformViaDataClass(origin, TypeDuo.build[D], wrap(f), eraseOriginIfDifferent = true)

      def using[D <: DataClass: WTT](f: O => D): Self2 = self2 :+
        CotransformViaDataClass(origin, TypeDuo.build[D], wrap(f), eraseOriginIfDifferent = false)

      // ---------------------------------------------------------------------------
      def as(resultingKey: KeyW) = new _As(resultingKey)
        class _As private[_CotransformViaDataClass] (resultingKey: KeyW) {

          def usingWithErasing[D: WTT](f: O => D): Self2 = self2 :+
            CotransformViaDataClassAs(origin, typeNode[D], resultingKey.value, wrap(f), eraseOriginIfDifferent = true)

          def using[D: WTT](f: O => D): Self2 = self2 :+
            CotransformViaDataClassAs(origin, typeNode[D], resultingKey.value, wrap(f), eraseOriginIfDifferent = false) } }

}

// ===========================================================================

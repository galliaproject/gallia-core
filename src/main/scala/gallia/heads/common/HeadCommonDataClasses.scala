package gallia
package heads
package common

import target.HT
import FunctionWrappers._
import actions.common.ActionsCommonDataClasses._

// ===========================================================================
trait HeadCommonDataClasses[F <: HeadCommon[F]] { ignored: HeadCommon[F] => // 220412171654

  // 220914145147 - transformDataClass
  // TODO: t220923095400 - in the future .transform(_.dataClass[Foo]("foo"))
  def transformDataClass[O: WTT](target: KeyW) = new { // TODO: better name?
    def using[D: WTT](f: O => D): Self2 = self2 :+
      TransformDataClass(target.value, from = HT.parse[O], to = HT.parse[D], wrap(f)) }

  // ===========================================================================
  /** does not *have* to be co-transform, can also only use one field (convenient for encapsulation) */
  // TODO: t220913144308 - macro versions
  def cotransformViaDataClass[O: WTT] = new _CotransformViaDataClass[O]() // TODO: better name? coin term?

    // ---------------------------------------------------------------------------
    class _CotransformViaDataClass[O: WTT] private[HeadCommonDataClasses] () {
      //TODO: t220801133413 - any way to forbid Option or Seq? at least as validation?

      // ---------------------------------------------------------------------------
      def usingWithErasing[D <: DataClass: WTT](f: O => D): Self2 = self2 :+
        CotransformViaDataClass(HT.parse[O], HT.parse[D], wrap(f), eraseOriginIfDifferent = true)

      def using[D <: DataClass: WTT](f: O => D): Self2 = self2 :+
        CotransformViaDataClass(HT.parse[O], HT.parse[D], wrap(f), eraseOriginIfDifferent = false)

      // ---------------------------------------------------------------------------
      def as(resultingKey: KeyW) = new {
        def usingWithErasing[D: WTT](f: O => D): Self2 = self2 :+
          CotransformViaDataClassAs(HT.parse[O], typeNode[D], resultingKey.value, wrap(f), eraseOriginIfDifferent = true)

        def using[D: WTT](f: O => D): Self2 = self2 :+
          CotransformViaDataClassAs(HT.parse[O], typeNode[D], resultingKey.value, wrap(f), eraseOriginIfDifferent = false) } }

}

// ===========================================================================

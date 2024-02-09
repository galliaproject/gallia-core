package gallia
package heads
package common

import plans.ActionMetaDag
import actions.common.ActionsCommonVeryBasics._
import actions.common.ActionsCommonUnionTypes
import actions.ActionsCustoms.CustomUU

// ===========================================================================
trait HeadCommon[F <: HeadCommon[F]]
      extends heads.Head                   [F]

      with    HeadCommonVeryBasics         [F]
      with    HeadCommonSomewhatBasics     [F]
      with    HeadCommonAsserts            [F]
      with    HeadCommonNestingRelated     [F]
      with    HeadCommonTransforms         [F]
      with    HeadCommonDataClasses        [F]
      with    HeadCommonCotransforms       [F] // TODO: t210826102833 - rework co-transforms
      with    HeadCommonFusion             [F]
      with    HeadCommonFission            [F]
      with    HeadCommonGenerusion         [F]
      with    HeadCommonGenerission        [F]
      with    HeadCommonMiscTransformations[F]
      with    HeadCommonTransformObjectIf  [F] {

    def fullDag: ActionMetaDag = env.Env.retrieveDagFromNode(nodeId)
    def showFullGraph(): Self2 = ???//fullDag.pipe(misc.CanDot.showGraph)
    // TODO: print

    // ---------------------------------------------------------------------------
    private[gallia] type Self2                = F
    private[gallia] val  self2: HeadCommon[F] = this

    protected[heads] def :+ (action: ActionUU): F

  // ===========================================================================
  def identity: Self2 = self2 :+ IdentityUU

  // ===========================================================================
  // custom: consider using 220412171654 instead (data classes
  // TODO: these will be very affected by t210104164036

    /** "Computer, I know better than you" */ def customO2ODataOnly(data: Obj => Obj) = self2 :+ new CustomUU(x => x, data)
    /** "Computer, I know better than you" */ def custom(x: ObjToObj)                 = self2 :+ new CustomUU(x.meta, x.data)

    @deprecated
    /** "Computer, I know better than you" */ def customU2U(meta: Cls => Cls, data: Obj => Obj) = self2 :+ new CustomUU(meta, data)
    /** "Computer, I know better than you" */ def customO2O(meta: Cls => Cls, data: Obj => Obj) = self2 :+ new CustomUU(meta, data)

  // ===========================================================================
  def showSchema()        :  Self2 = self2 :+ ShowSchema(abort = false)
  def showSchemaAndAbort():  Self2 = self2 :+ ShowSchema(abort = true )

  // ===========================================================================
  def modifyEnumValuesFor(target1: RPathW, more: RPathW*)            : _ModifyEnumValuesFor = modifyEnumValuesFor(_.explicit(target1 -> more))
  def modifyEnumValuesFor(targets: RPathWz)                          : _ModifyEnumValuesFor = modifyEnumValuesFor(_.explicit(targets))
  def modifyEnumValuesFor(selector: SEL.ModifyEnumValuesFor.Selector): _ModifyEnumValuesFor = new _ModifyEnumValuesFor(SEL.ModifyEnumValuesFor.resolve(selector))

    // ---------------------------------------------------------------------------
    class _ModifyEnumValuesFor(target: TqRPathz) {
      def add   (value: EnumStringValue)          : Self2 = using(_ :+             EnumValue(value))
      def remove(value: EnumStringValue)          : Self2 = using(_.filterNot(_ == EnumValue(value)))

      // ---------------------------------------------------------------------------
      def using(f: Seq[EnumValue] => Seq[EnumValue]): Self2 =
        self2 :+ actions.ActionsOthers.ModifyEnumValuesFor(target, f) }

  // ===========================================================================
  private[gallia] def validateBObj (value: BObj ): Self2 = self2 :+ new ValidateBObj (value)
  private[gallia] def validateBObjs(value: BObjs): Self2 = self2 :+ new ValidateBObjs(value)

  // ===========================================================================
  def fuseToUnion(origin1: KeyW, origin2: KeyW) = new _FuseToUnion(origin1, origin2)
    final class _FuseToUnion private[heads] (origin1: KeyW, origin2: KeyW) {
      def as(union: KeyW): Self2 = self2 :+
        ActionsCommonUnionTypes.FuseToUnion(origin1.value, origin2.value, union.value) }

    // ---------------------------------------------------------------------------
    def fissionFromUnion(origin: KeyW) = new _FissionFromUnion(origin)
     final class _FissionFromUnion private[heads] (origin: KeyW) {
      def as(target1: KeyW, target2: KeyW): Self2 = self2 :+
        ActionsCommonUnionTypes.FissionFromUnion(origin.value, target1.value, target2.value) }

  // ===========================================================================
  // TODO: t210127164512
  /*
    def isPresent(key: KeyW): HeadV[Boolean] = ???
    def isMissing(key: KeyW): HeadV[Boolean] = ???
    def unquoteKeys: Self2 = ??? // pretty common
  */

}

// ===========================================================================

package gallia
package heads.common

import env.ActionDag
import actions.ActionsUUVeryBasics._

// ===========================================================================
trait HeadCommon[F <: HeadCommon[F]]
      extends heads.Head               [F]

      with    HeadCommonVeryBasics         [F]
      with    HeadCommonSomewhatBasics     [F]
      with    HeadCommonAsserts            [F]
      with    HeadCommonNestingRelated     [F]
      with    HeadCommonTransforms         [F]
      with    HeadCommonCotransforms       [F] // TODO: t210826102833 - rework co-transforms
      with    HeadCommonFusion             [F]
      with    HeadCommonFission            [F]
      with    HeadCommonGenerusion         [F]
      with    HeadCommonGenerission        [F]
      with    HeadCommonMiscTransformations[F] {

    def fullDag: ActionDag = env.Env.retrieveDagFromNode(nodeId)
    def showFullGraph(): Self2 = ???//fullDag.pipe(misc.CanDot.showGraph)
    // TODO: print

    // ---------------------------------------------------------------------------
    private[gallia] type Self2                = F
    private[gallia] val  self2: HeadCommon[F] = this

    protected[heads] def :+ (action: ActionUU): F

  // ===========================================================================
  def identity: Self2 = self2 :+ IdentityUU

  // ---------------------------------------------------------------------------
  // TODO: this will be very affected by t210104164036
  /** "Computer, I know better than you" */
  def customU2U(meta: Cls => Cls, data: Obj => Obj) = self2 :+ new actions.ActionsCustoms.CustomUU(meta, data)

  // ---------------------------------------------------------------------------
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
  // TODO: t210127164512
  /*
    def isPresent(key: KeyW): HeadV[Boolean] = ???
    def isMissing(key: KeyW): HeadV[Boolean] = ???
    def unquoteKeys: Self2 = ??? // pretty common
  */

}

// ===========================================================================

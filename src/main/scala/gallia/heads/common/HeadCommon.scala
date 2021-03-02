package gallia.heads.common

import gallia._
import gallia.env.ActionDag
import gallia.actions.ActionsUUVeryBasics._

// ===========================================================================
trait HeadCommon[F <: HeadCommon[F]]
      extends gallia.heads.Head               [F]

      with    HeadCommonVeryBasics         [F]
      with    HeadCommonSomewhatBasics     [F]
      with    HeadCommonAsserts            [F]
      with    HeadCommonNestingRelated     [F]
      with    HeadCommonTransforms         [F]
      with    HeadCommonCotransforms       [F]
      with    HeadCommonFuseFission        [F]
      with    HeadCommonGenerates          [F]
      with    HeadCommonMiscTransformations[F] {

    def fullDag: ActionDag = gallia.env.Env.retrieveDagFromNode(nodeId)
    def showFullGraph(): Self2 = ???//fullDag.thn(misc.CanDot.showGraph)
    // TODO: print

    // ---------------------------------------------------------------------------
    private[gallia] type Self2 = F
    private[gallia] val  self2  = this

    protected[heads] def :+ (action: ActionUU): F

  // ===========================================================================
  def identity: Self2 = self2 :+ IdentityUU

  // ---------------------------------------------------------------------------
  // TODO: this will be very affected by t210104164036
  /** "Computer, I know better than you" */
  def customUU(meta: Cls => Cls, data: Obj => Obj) = self2 :+ new gallia.actions.ActionsCustoms.CustomUU(meta, data)

  // ---------------------------------------------------------------------------
  def showSchema        :  Self2 = self2 :+ ShowSchema(abort = false)
  def showSchemaAndAbort:  Self2 = self2 :+ ShowSchema(abort = true )

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

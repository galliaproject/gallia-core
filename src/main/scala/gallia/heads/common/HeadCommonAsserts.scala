package gallia
package heads
package common

import meta._
import reflect.{BasicType, Container}
import actions.ActionsCustoms.{CustomMeta, CustomField}
import actions.ActionsAsserts._

// ===========================================================================
@PartialTypeMatching
trait HeadCommonAsserts[F <: HeadCommon[F]] { ignored: HeadCommon[F] =>

  def assertMeta(f: ClsLike => Boolean): Self2 = self2 :+ AssertMeta(f)

    //TODO: field selection too? or for-key...
    def assertField(target: KPathW) = new {
        def matches(pred: InfoLike => Boolean): Self2 =
          self2 :+ AssertField(target.value, _Error.FieldAssertionFailure(target.value), pred) }

      private def _assertContainer(target: KPath, container: Container): Self2 = self2 :+ AssertContainer(target, container)
        def assertIsOne   (target: KPathW): Self2 = _assertContainer(target.value, Container._One)
        def assertIsOpt   (target: KPathW): Self2 = _assertContainer(target.value, Container._Opt)
        def assertIsNes   (target: KPathW): Self2 = _assertContainer(target.value, Container._Nes)
        def assertIsPes   (target: KPathW): Self2 = _assertContainer(target.value, Container._Pes)

      // TODO: t210129093837 - assert is distinct

      private def _assertBasicType(target: KPath, basicType: BasicType): Self2 = self2 :+ AssertBasicType(target, basicType)
        def assertIsString (target: KPathW): Self2 = _assertBasicType(target.value, BasicType._String)
        def assertIsInt    (target: KPathW): Self2 = _assertBasicType(target.value, BasicType._Int)
        def assertIsDouble (target: KPathW): Self2 = _assertBasicType(target.value, BasicType._Double)
        def assertIsBoolean(target: KPathW): Self2 = _assertBasicType(target.value, BasicType._Boolean)

      // TODO: t210129094319 - def assertIsDistinct(target: KPathW): Self2 = transformObjects(target.value).using(_.force.distinct) // TODO: dedicated - not necessarily objects...
      // TODO: assert sizes

  // ---------------------------------------------------------------------------
  def assertIsUnionType (target: KPathW): Self2 = self2 :+ AssertUnionType(target.value, negated = false)
  def assertNotUnionType(target: KPathW): Self2 = self2 :+ AssertUnionType(target.value, negated = true)

  // ---------------------------------------------------------------------------
  def assertNonEmptyString(target: KPathW): Self2 = assertString(target).isNonEmpty

  def assertString(target: KPathW) = new {
    def isNonEmpty: Self2 = assertDataU(_.stringx(target)).using(_.nonEmpty)

    def startsWith    (prefix: String)                : Self2 = assertDataU(_.stringx(target)).using(_.startsWith(prefix))
    def   endsWith    (suffix: String)                : Self2 = assertDataU(_.stringx(target)).using(_.endsWith  (suffix))
    def surroundedWith(prefix: String, suffix: String): Self2 = assertDataU(_.stringx(target)).using(x => x.startsWith(prefix) && x.endsWith(suffix))
    def matchesRegex  (regex : Regex)                 : Self2 = assertDataU(_.stringx(target)).using(regex
      // matches - causes issues with scala 2.12: "value matches is not a member of scala.util.matching.Regex"
      .pattern.matcher(_).matches()) }

  // ---------------------------------------------------------------------------
  def customMeta(f: Cls => Cls): Self2 = self2 :+ CustomMeta(f)

    // TODO: or customInfo?
    private[heads] def customField(target: TqRPathz)         = new _CustomField(target)
      def customField(x : RPathW)                            = new _CustomField(RPathWz.from(x))
      def customField(x1: RPathW, x2: RPathW, more: RPathW*) = new _CustomField(x1, x2, more)
      def customField(xs: RPathWz)                           = new _CustomField(xs)
      def customField(sel: SEL.Custom.Selector)              = new _CustomField(SEL.Custom.resolve(sel))

      // ---------------------------------------------------------------------------
      class _CustomField(target: TqRPathz) {
        def using(f: Info => Info): Self2 = self2 :+ CustomField(target, f) }

  // ---------------------------------------------------------------------------
  import TSL.AssertData._

  def assertDataUnsafeU(valid: Obj => Boolean): Self2 = self2 :+ AssertDataUnsafeU(valid)

    @Max5
    def assertDataU[T1: WTT]                  (f1: AssertData[T1])                                         = new { def using(f:  T1          => Boolean): Self2 = self2 :+ AssertDataU1(resolve (f1    ), f) }
    def assertDataU[T1: WTT, T2: WTT]         (f1: AssertData[T1], f2: AssertData[T2])                     = new { def using(f: (T1, T2)     => Boolean): Self2 = self2 :+ AssertDataU2(resolve2(f1, f2), f) }
    def assertDataU[T1: WTT, T2: WTT, T3: WTT](f1: AssertData[T1], f2: AssertData[T2], f3: AssertData[T3]) = new { def using(f: (T1, T2, T3) => Boolean): Self2 = ??? }

  def assertDataClass[DC: WTT]: Self2 = self2 :+ AssertDataClass(typeNode[DC])

  // ---------------------------------------------------------------------------
  // additional validation
  /*protected[gallia] */def ensureNumerical0(key: KeyW) = self2 :+ actions.common.ActionsCommonSomewhatBasics.EnsureNumeric(TargetQueryUtils.tqkpathz(key)) // TODO: keep? expand?
}

// ===========================================================================

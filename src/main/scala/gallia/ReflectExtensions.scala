package gallia

import meta._
import reflect.{TypeNode, TypeLeaf, Container}

// ===========================================================================
/** so we could externalize gallia-reflect */
trait ReflectExtensions {

  // ---------------------------------------------------------------------------
  implicit class TypeNode_(node: TypeNode) {
    def formatDefault: String = obj.formatPrettyJson
    def obj          : Obj    = reflect.TypeNodeObj.typeNode(node)

    // eg for translate type mismatch
    def formatSuccinct: String = reflect.TypeNodeUtils.formatSuccinct(node)

    // ---------------------------------------------------------------------------
    def forceNonBObjInfo                    : Info    = node.ensuring(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjInfo)
    def forceNonBObjSubInfo                 : SubInfo = node.ensuring(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjSubInfo)
    def forceNonBObjSubInfo(enmOpt: _EnmOpt): SubInfo = node.ensuring(!_.isContainedBObj).pipe(InfoUtils.forceNonBObjSubInfo(enmOpt))

    // ---------------------------------------------------------------------------
    def nodeDesc = reflect.NodeDesc.from(node)

    // ---------------------------------------------------------------------------
    def enmInfo1(c: Cls, path: KPath, multiple: Multiple): Info1 =
      c .field(path)
        .enmValueType(multiple)
        .pipe(node.containerType.info1) }

  // ===========================================================================
  implicit class TypeLeaf_(leaf: TypeLeaf) {
    def enumeratumEnum: Seq[EnumValue] = leaf.enumeratumValueNamesOpt.get.map(EnumValue.apply)

    // ---------------------------------------------------------------------------
    def forceDataClass: Cls =
        dataClassEither match {
          case Left (l) => aptus.illegalArgument(l)
          case Right(r) => r }

      // ---------------------------------------------------------------------------
      def dataClassEither: Either[Any, Cls] = // TODO: use Try until determine precise criteria (cc + not Some + valid types...)
        util.Try(meta.InfoUtils.forceNestedClass(leaf)) match {
          case util.Failure(error)          => Left(s"TODO:t201015102536:${error.toString}")
          case util.Success(validDataClass) => Right(validDataClass) } }

  // ===========================================================================
  implicit class Container_(container: Container) {
    def info(valueType: meta.ValueType): Info =
      container match {
        case Container._One => Info.one(valueType)
        case Container._Opt => Info.opt(valueType)
        case Container._Nes => Info.nes(valueType)
        case Container._Pes => Info.pes(valueType) }

    // ---------------------------------------------------------------------------
    def info1(valueType: meta.ValueType): Info1 =
      container match {
        case Container._One => Info1.one(valueType)
        case Container._Opt => Info1.opt(valueType)
        case Container._Nes => Info1.nes(valueType)
        case Container._Pes => Info1.pes(valueType) } } }

// ===========================================================================
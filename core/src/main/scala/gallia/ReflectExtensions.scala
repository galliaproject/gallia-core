package gallia
package reflect

import aptus.{Anything_, String_, Throwable_}
import meta._

// ===========================================================================
/** so we could externalize gallia-reflect */
trait ReflectExtensions {

  // ---------------------------------------------------------------------------
  implicit class TypeNode_(dis: TypeNode) {
    def formatDefault: String = obj.formatPrettyJson
    def obj          : Obj    = reflect.TypeNodeObj.typeNode(dis)

    // ---------------------------------------------------------------------------
    // eg for translate type mismatch
    def formatSuccinct: String = {
        def tmp(value: TypeLeaf) = value.name

        def isTopLevel(value: TypeNode): Boolean =
          !value.complex && basic.BasicType.isKnown(value.leaf.name)

        // ---------------------------------------------------------------------------
        dis.containerTypeOpt match {
          case Some(container) =>
            val leaf = dis.args.head.leaf.pype(tmp)

            "succinct".colon(container match {
                case Container._Opt => leaf.surroundWith(    "Option[", "]")
                case Container._Nes => leaf.surroundWith(       "Seq[", "]")
                case Container._Pes => leaf.surroundWith("Option[Seq[", "]]")
                case _              => ??? }) // TODO:can't actually happen - create sub-enum

          case None =>
            if (isTopLevel(dis)) s"succinct:${tmp(dis.leaf)}"
            else                 dis.formatDefault } }

    // ---------------------------------------------------------------------------
    def forceNonBObjInfo                    : Info    = dis.ensuring(!_.isContainedBObj).pype(InfoUtils.forceNonBObjInfo)
    def forceNonBObjSubInfo                 : SubInfo = dis.ensuring(!_.isContainedBObj).pype(InfoUtils.forceNonBObjSubInfo)
    def forceNonBObjSubInfo(enmOpt: _EnmOpt): SubInfo = dis.ensuring(!_.isContainedBObj).pype(InfoUtils.forceNonBObjSubInfo(enmOpt))

    // ---------------------------------------------------------------------------
    def nodeDesc = reflect.NodeDesc.from(dis)

    // ---------------------------------------------------------------------------
    def enmInfo1(c: Cls, path: KPath, multiple: Multiple): Info1 =
      c .field(path)
        .enmValueType(multiple)
        .pype(dis.containerType.info1) }

  // ===========================================================================
  implicit class TypeLeaf_(leaf: TypeLeaf) {
    def formatDefault: String = obj.formatPrettyJson
    def obj          : Obj    = reflect.TypeNodeObj.typeLeaf(leaf)

    // ---------------------------------------------------------------------------
    def enumeratumEnum: Seq[EnumValue] = leaf.enumeratumValueNamesOpt.get.map(EnumValue.apply)

    // ---------------------------------------------------------------------------
    def forceDataClass: Cls =
        dataClassEither match {
          case Left (l) => aptus.illegalArgument(l)
          case Right(r) => r }

      // ---------------------------------------------------------------------------
      def dataClassEither: Either[Any, Cls] = // TODO: use Try until determine precise criteria (cc + not Some + valid types...)
        util.Try(meta.InfoUtils.forceNestedClass(leaf)) match {
          case util.Failure(error)          => Left(s"TODO:NotADataClass:t201015102536:${error.getMessage}:${error.formatStackTrace}")
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
        case Container._Pes => Info1.pes(valueType) } }


  // ===========================================================================
  private[gallia] implicit class DynamicToStatic_(instantiator: Instantiator) {

    def dynamicToStatic(value: Any)(subInfo: Info): Any = {
           if (subInfo.isOne) { value.asInstanceOf[              Obj  ]      .pype(this.instantiateStaticRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isOpt) { value.asInstanceOf[       Option[Obj] ]      .map (this.instantiateStaticRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isNes) { value.asInstanceOf[       Seq   [Obj] ]      .map (this.instantiateStaticRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isPes) { value.asInstanceOf[Option[Seq   [Obj]]].map(_.map (this.instantiateStaticRecursively(subInfo.forceNestedClass))) }
      else ??? } // TODO: as match rather

    // ===========================================================================
    private[DynamicToStatic_] def instantiateStaticRecursively(c: Cls)(o: Obj): Any =
        c .fields // for order
          .map (processField(o))
          .pype(instantiator.construct)

      // ---------------------------------------------------------------------------
      private def processField(o: Obj)(field: Fld): AnyRef =
          (field.nestedClassOpt match {
              case None =>
                if (field.isRequired) o.forceKey  (field.key)
                else                  o.attemptKey(field.key)
              case Some(nc) => processContainedObj(nc, field, o) })
            .asInstanceOf[AnyRef /* TODO: safe? */]

        // ---------------------------------------------------------------------------
        private def processContainedObj(c2: Cls, field: Fld, o: Obj): Any =
            field.info.container1 match { // TODO: use Container.wrap now?
              case Container._One => o.forceKey  (field.key)                            .pype(processObj(c2, field))
              case Container._Opt => o.attemptKey(field.key)                            .map (processObj(c2, field))
              case Container._Nes => o.forceKey  (field.key)      .asInstanceOf[List[_]].map (processObj(c2, field))
              case Container._Pes => o.attemptKey(field.key).map(_.asInstanceOf[List[_]].map (processObj(c2, field))) }

          // ---------------------------------------------------------------------------
          import gallia.DynamicToStatic_ // only needed for scala 3 (not sure why)
          private def processObj(nc: Cls, field: meta.Fld)(value: Any): Any =
            instantiator
              .nesting(field.skey) // guaranteed if nested class
              .instantiateStaticRecursively(nc)(
                  value.asInstanceOf[Obj] /* by design if passed validation */) }

  // ===========================================================================
  object StaticToDynamic {

    def apply(value: Any)(subInfo: Info): Any = {
             if (subInfo.isOne) { val c2 = subInfo.forceNestedClass; staticToObj  (c2)(value) }
        else if (subInfo.isOpt) { val c2 = subInfo.forceNestedClass; staticToObj_ (c2)(value) }
        else if (subInfo.isNes) { val c2 = subInfo.forceNestedClass; staticToObjs (c2)(value) }
        else if (subInfo.isPes) { val c2 = subInfo.forceNestedClass; staticToObjs_(c2)(value) }
        else ??? } // TODO: (match rather)

      // ---------------------------------------------------------------------------
              def staticToObj  (c: Cls)(value: Any): Any = value.asInstanceOf[              Product  ]            .productIterator.pype(valuesToObjOpt(c)).getOrElse(None)
      private def staticToObj_ (c: Cls)(value: Any): Any = value.asInstanceOf[       Option[Product] ]      .map(_.productIterator.pype(valuesToObjOpt(c)).getOrElse(None))
              def staticToObjs (c: Cls)(value: Any): Any = value.asInstanceOf[       Seq   [Product] ]      .map(_.productIterator.pype(valuesToObjOpt(c)).getOrElse(None))
      private def staticToObjs_(c: Cls)(value: Any): Any = value.asInstanceOf[Option[Seq   [Product]]].map(_.map(_.productIterator.pype(valuesToObjOpt(c)).getOrElse(None)))

        // ---------------------------------------------------------------------------
        private def valuesToObjOpt(c: Cls)(itr: Iterator[AnyValue]): Option[Obj] =
          c .fields
            .map { field =>
              field.key ->
                potentiallyProcessNesting(field.info)(
                  value = itr.next()) }
            .flatMap(data.single.ObjIn.normalizeEntry)
            .in.noneIf(_.isEmpty)
            .map(obj)
            .tap(_ => assert(itr.isEmpty, c /* TODO: pass original value? */)) }

    // ===========================================================================
    private def potentiallyProcessNesting(info: Info)(value: AnyValue): AnyValue =
      info
        .nestedClassOpt
        .map { nestedClass =>
          info
            .container1
            .containerWrap(f = StaticToDynamic.staticToObj(nestedClass))
            .apply(value) }
        .getOrElse(value) }

// ===========================================================================

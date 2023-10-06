package gallia
package target

import meta._
import aptus.Anything_

// ===========================================================================
class Instantiator private[gallia] (
      target     : aptus.DebugString,

      nestedObjs : Map[Key, Instantiator],
      klass      : java.lang.reflect.Constructor[_]) {

    override def toString: String = formatDefault
      def formatDefault: String =
        s"Instantiator(${target})"

    // ===========================================================================
    def in(value: Any)(subInfo: Info): Any = {
           if (subInfo.isOne) { value.asInstanceOf[              Obj  ]      .pipe(this.instantiateRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isOpt) { value.asInstanceOf[       Option[Obj] ]      .map (this.instantiateRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isNes) { value.asInstanceOf[       Seq   [Obj] ]      .map (this.instantiateRecursively(subInfo.forceNestedClass))  }
      else if (subInfo.isPes) { value.asInstanceOf[Option[Seq   [Obj]]].map(_.map (this.instantiateRecursively(subInfo.forceNestedClass))) }
      else ??? } // TODO: as match rather

    // ===========================================================================
    private def instantiateRecursively(c: Cls)(o: Obj): Any =
        c .fields // for order
          .map(processField(o))
          .pipe { args => klass.newInstance(args:_*) }

      // ---------------------------------------------------------------------------
      private def processField(o: Obj)(field: Fld): AnyRef =
          (field.nestedClassOpt match {
              case None =>
                if (field.isRequired) o.forceKey  (field.key)
                else                  o.attemptKey(field.key)
              case Some(c2) => processContainedObj(c2, field, o) })
            .asInstanceOf[AnyRef /* TODO: safe? */]

        // ---------------------------------------------------------------------------
        private def processContainedObj(c2: Cls, field: Fld, o: Obj): AnyValue =
            field.info.container1 match { // TODO: use Container.wrap now?
              case Container._One => o.forceKey  (field.key)                            .pipe(processObj(c2, field))
              case Container._Opt => o.attemptKey(field.key)                            .map (processObj(c2, field))
              case Container._Nes => o.forceKey  (field.key)      .asInstanceOf[List[_]].map (processObj(c2, field))
              case Container._Pes => o.attemptKey(field.key).map(_.asInstanceOf[List[_]].map (processObj(c2, field))) }

          // ---------------------------------------------------------------------------
          private def processObj(c2: Cls, field: gallia.meta.Fld)(value: AnyValue): Any =
            nestedObjs
              .apply(field.key) // guaranteed if nested class
              .instantiateRecursively(c2)(
                  value.asInstanceOf[Obj] /* by design if passed validation */) }

  // ===========================================================================
  object Instantiator {

    def out(value: Any)(subInfo: Info): Any = {
             if (subInfo.isOne) { val c2 = subInfo.forceNestedClass; Instantiator.valueToObj  (c2)(value) }
        else if (subInfo.isOpt) { val c2 = subInfo.forceNestedClass; Instantiator.valueToObj_ (c2)(value) }
        else if (subInfo.isNes) { val c2 = subInfo.forceNestedClass; Instantiator.valueToObjs (c2)(value) }
        else if (subInfo.isPes) { val c2 = subInfo.forceNestedClass; Instantiator.valueToObjs_(c2)(value) }
        else ??? } // TODO: (match rather)

      // ---------------------------------------------------------------------------
              def valueToObj  (c: Cls)(value: Any): Any = value.asInstanceOf[              Product  ]            .productIterator.pipe(valuesToObjOpt(c)).getOrElse(None)
      private def valueToObj_ (c: Cls)(value: Any): Any = value.asInstanceOf[       Option[Product] ]      .map(_.productIterator.pipe(valuesToObjOpt(c)).getOrElse(None))
              def valueToObjs (c: Cls)(value: Any): Any = value.asInstanceOf[       Seq   [Product] ]      .map(_.productIterator.pipe(valuesToObjOpt(c)).getOrElse(None))
      private def valueToObjs_(c: Cls)(value: Any): Any = value.asInstanceOf[Option[Seq   [Product]]].map(_.map(_.productIterator.pipe(valuesToObjOpt(c)).getOrElse(None)))

        // ---------------------------------------------------------------------------
        private def valuesToObjOpt(c: Cls)(itr: Iterator[AnyValue]): Option[Obj] =
          c .fields
            .map { field =>
              field.key ->
                field.info.potentiallyProcessNesting(
                  value = itr.next()) }
            .flatMap(data.single.ObjIn.normalizeEntry)
            .in.noneIf(_.isEmpty)
            .map(obj)
            .tap(_ => assert(itr.isEmpty, c /* TODO: pass original value? */)) }

// ===========================================================================

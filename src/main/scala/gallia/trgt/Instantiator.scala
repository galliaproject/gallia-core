package gallia
package target

import scala.reflect.runtime.universe

import aptus.{String_, Seq_}
import aptus.DebugString

import meta._

// ===========================================================================
class Instantiator private (
      target: DebugString,

      nestedObjs : Map[Key, Instantiator],
      klass      : java.lang.reflect.Constructor[_]) {

    override def toString: String = formatDefault
      def formatDefault: String = s"Instantiator(${target})"

    // ---------------------------------------------------------------------------
    def instantiateRecursively(c: Cls, o: Obj): Any =
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
            case Container._Opt => o.attemptKey(field.key)                            .map (processObj(c2, field))
            case Container._Pes => o.attemptKey(field.key).map(_.asInstanceOf[List[_]].map (processObj(c2, field)))
            case Container._One => o.forceKey  (field.key)                            .pipe(processObj(c2, field))
            case Container._Nes => o.forceKey  (field.key)      .asInstanceOf[List[_]].map (processObj(c2, field)) }

          // ---------------------------------------------------------------------------
          private def processObj(c2: Cls, field: gallia.meta.Fld)(value: AnyValue): Any =
            nestedObjs
              .apply(field.key) // guaranteed if nested class
              .instantiateRecursively(
                  c2,
                  value.asInstanceOf[Obj] /* by design if passed validation */)
  }

  // ===========================================================================
  object Instantiator {

    def fromFirstTypeArgFirstTypeArg[T: WTT]: Instantiator =
        scala.reflect.runtime.universe
          .weakTypeTag[T]
          .pipe { tag => reflect.InstantiatorUtils.rec(new Instantiator(_, _, _))(tag.mirror)(tag.tpe.typeArgs.head.typeArgs.head) }

    // ---------------------------------------------------------------------------
    def fromFirstTypeArg[T: WTT]: Instantiator =
        scala.reflect.runtime.universe
          .weakTypeTag[T]
          .pipe { tag => reflect.InstantiatorUtils.rec(new Instantiator(_, _, _))(tag.mirror)(tag.tpe.typeArgs.head) }

    // ---------------------------------------------------------------------------
    def fromType[T: WTT]: Instantiator =
        scala.reflect.runtime.universe
          .weakTypeTag[T]
          .pipe { tag => reflect.InstantiatorUtils.rec(new Instantiator(_, _, _))(tag.mirror)(tag.tpe) } }

// ===========================================================================

package gallia.target

import scala.reflect.runtime.universe

import aptus.{String_, Seq_}

import gallia._
import gallia.meta._
import aptus.DebugString

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
              if (field.info.isRequired) o.force(field.key)
              else                       o.opt  (field.key)
            case Some(c2) => processContainedObj(c2, field, o) })
          .asInstanceOf[AnyRef /* TODO: safe? */]

        // ---------------------------------------------------------------------------
        private def processContainedObj(c2: Cls, field: Fld, o: Obj): AnyValue =
          field.info.container match { // TODO: use Container.wrap now?
            case Container._Opt => o.opt  (field.key)                           .map(processObj(c2, field))
            case Container._Pes => o.opt  (field.key).map(_.asInstanceOf[Seq[_]].map(processObj(c2, field)))
            case Container._One => o.force(field.key)                           .pipe(processObj(c2, field))
            case Container._Nes => o.force(field.key)      .asInstanceOf[Seq[_]].map(processObj(c2, field)) }

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
          .pipe { tag => rec(tag.mirror)(tag.tpe.typeArgs.head.typeArgs.head) }

    // ---------------------------------------------------------------------------
    def fromFirstTypeArg[T: WTT]: Instantiator =
        scala.reflect.runtime.universe
          .weakTypeTag[T]
          .pipe { tag => rec(tag.mirror)(tag.tpe.typeArgs.head) }

    // ---------------------------------------------------------------------------
    def fromType[T: WTT]: Instantiator =
        scala.reflect.runtime.universe
          .weakTypeTag[T]
          .pipe { tag => rec(tag.mirror)(tag.tpe) }

      // ===========================================================================
      private def rec(mirror: universe.Mirror)(tpe: universe.Type): Instantiator = {

        val methodSymbols = // TODO: refactor with other reflect's occurence
          tpe
            .decls
            .filter((x: scala.reflect.api.Symbols#SymbolApi) => x.isMethod)
            .map   (_.asMethod)
            .filter(_.isCaseAccessor)
            .toSeq

        // ---------------------------------------------------------------------------
        val node = gallia.reflect.TypeNode.parse(tpe)

        val nestedObjs: Map[Key, Instantiator] =
           methodSymbols
            .zip(node.leaf.fields)
            .flatMap { case (methodSymbol, f) =>
              if (!f.node.isContainedDataClass) None
              else
                methodSymbol
                  .typeSignature
                  .resultType
                  .pipe(subInstantiator(mirror, f.node.containerType, _))
                  .pipe(instantiator => Some(f.key.symbol -> instantiator)) }
            .force.map

        // ---------------------------------------------------------------------------
        new Instantiator(
            target = node.leaf.name, // for debugging only
            nestedObjs,
            mirror.runtimeClass(tpe).mainConstructor)
      }

      // ---------------------------------------------------------------------------
      private def subInstantiator(mirror: universe.Mirror, containerType: Container, resultType: universe.Type): Instantiator =
        containerType match {
            case Container._One => rec(mirror)(resultType)
            case Container._Pes => rec(mirror)(resultType.typeArgs.head.typeArgs.head)
            case Container._Opt => rec(mirror)(resultType.typeArgs.head)
            case Container._Nes => rec(mirror)(resultType.typeArgs.head) }

    // ===========================================================================
    implicit class Class__(u: Class[_]) {
      def mainConstructor =
        u
          .getConstructors
          .headOption // t200720101733 - establish always safe or add corresponding validation
          .getOrElse(null) // TODO: handle better (happens with eg .removeIf('f).hasValue(None))
    }

  }

// ===========================================================================

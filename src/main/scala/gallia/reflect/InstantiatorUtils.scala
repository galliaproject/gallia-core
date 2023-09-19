package gallia
package reflect

import aptus.{String_, Seq_}
import scala.reflect.runtime.universe

// ===========================================================================
object InstantiatorUtils {

  type InstantiatorContructor[$Instantiator] =
    (aptus.DebugString, Map[Key, $Instantiator], java.lang.reflect.Constructor[_]) =>
      $Instantiator

  // ===========================================================================
  def rec[$Instantiator]
          /* basically new Instantiator(_, _, _) */
          (constr: InstantiatorContructor[$Instantiator])
          (mirror: universe.Mirror)
          (tpe   : universe.Type) // that we want to instantiate
        : $Instantiator = {
      val node = gallia.reflect.TypeNode.parse(tpe)

      val nestedObjs: Map[Key, $Instantiator] =
        ReflectUtils
          .methodSymbols(tpe)
          .zip(node.leaf.fields)
          .flatMap { case (methodSymbol, field) =>
            if (!field.node.isContainedDataClass) None
            else
              methodSymbol
                .typeSignature
                .resultType
                .pipe(subInstantiator(constr)(field.node.containerType, mirror, _))
                .pipe(instantiator => Some(field.key.symbol -> instantiator)) }
          .force.map

      // ---------------------------------------------------------------------------
      constr(
          node.leaf.name, // for debugging only
          nestedObjs,
          mirror.runtimeClass(tpe).mainConstructor)
    }

    // ===========================================================================
    private def subInstantiator[$Instantiator](constr: InstantiatorContructor[$Instantiator])(
          containerType: Container,
          mirror       : universe.Mirror,
          resultType   : universe.Type)
        : $Instantiator =
      containerType match {
          case Container._One => rec(constr)(mirror)(resultType)
          case Container._Pes => rec(constr)(mirror)(resultType.typeArgs.head.typeArgs.head)
          case Container._Opt => rec(constr)(mirror)(resultType.typeArgs.head)
          case Container._Nes => rec(constr)(mirror)(resultType.typeArgs.head) }

  // ===========================================================================
  private implicit class Class__(u: Class[_]) {
    def mainConstructor =
      u .getConstructors
        .headOption // t200720101733 - establish always safe or add corresponding validation
        .getOrElse(null) } // TODO: handle better (happens with eg .removeIf('f).hasValue(None))

}

// ===========================================================================
package gallia
package reflect
package macros3

import aptus.Seq_

// ===========================================================================
private object InstantiatorCreatorMacro3 {
  import scala.quoted.{Quotes, quotes, Type, Expr}

  // ===========================================================================
  private object MethodNames {
    val arbitraryClosureName: String = "myClosure"

    def entry             : String =  "entry"
    def entries(size: Int): String = s"entries${size}"
    def entriesMap        : String =  "entriesMap"

    val enumeratumWithName: String = "withName"
    val caseClassApply    : String = "apply"

    val instanEnumeratumWithName: String =  "enumeratumWithName" /* 231215104241 - shouldn't be renamed */
    def instanFrom(size: Int)   : String = s"from${size}"        /* 231215104241 - shouldn't be renamed */ }

  // ===========================================================================
  private object HelperMethods { /* don't rename methods, see 231215104241 */
    type E = Tuple2[String, Instantiator]

    // ---------------------------------------------------------------------------
    def entry  (key: String, value: Instantiator): E = Tuple2[String, Instantiator](key, value)

    //def entries(values: List[E]): List[E] = values

    def entriesMap(values: List[E]): Map[String, Instantiator] = values.force.map

    // ---------------------------------------------------------------------------
    //FIXME: t231221110951
    def entries0()                                 : List[E] = List()
    def entries1(v1: E)                            : List[E] = List(v1)
    def entries2(v1: E, v2: E)                     : List[E] = List(v1, v2)
    def entries3(v1: E, v2: E, v3: E)              : List[E] = List(v1, v2, v3)
    def entries4(v1: E, v2: E, v3: E, v4: E)       : List[E] = List(v1, v2, v3, v4)
    def entries5(v1: E, v2: E, v3: E, v4: E, v5: E): List[E] = List(v1, v2, v3, v4, v5)

    def entries10(v1: E, v2: E, v3: E, v4: E, v5: E, v6: E, v7: E, v8: E, v9: E, vA: E): List[E] = List(v1, v2, v3, v4, v5, v6, v7, v8, v9, vA)

def entriesN(vs: E*): List[E] = vs.toList
//wrong number of arguments at pickler for (vs: gallia.reflect.reflect3.InstantiatorCreator.HelperMethods.E*):
//[error]    |  List[gallia.reflect.reflect3.InstantiatorCreator.HelperMethods.E]: (gallia.reflect.reflect3.InstantiatorCreator.HelperMethods#entriesN :
//[error]    |  (vs: gallia.reflect.reflect3.InstantiatorCreator.HelperMethods.E*):
//[error]    |    List[gallia.reflect.reflect3.InstantiatorCreator.HelperMethods.E]
//[error]    |), expected: 1, found: 0
  }

  // ===========================================================================
  def rec(using q: Quotes)(tpe: q.reflect.TypeRepr)(typeNode: TypeNode): q.reflect.Term = {
    import quotes.reflect.*

         if (!typeNode.isContainedDataClass && !typeNode.isContainedEnumeratum) '{Instantiator.Placeholder /* easier for now */}.asTerm
    else if (typeNode.isOptionOfSeq) rec(tpe.typeArgs.force.one.typeArgs.force.one)(typeNode.forceSoleTypeArg.forceSoleTypeArg)
    else if (typeNode.isOption)      rec(tpe.typeArgs.force.one)                   (typeNode.forceSoleTypeArg)
    else if (typeNode.        isSeq) rec(tpe.typeArgs.force.one)                   (typeNode.forceSoleTypeArg)
    else {
      val enumeratum = typeNode.leaf.isEnumeratum

      val caseFieldSymbols: List[Symbol]   = tpe.typeSymbol.caseFields
      val typeReprs       : List[TypeRepr] = if (enumeratum) List(TypeRepr.of[String]) else caseFieldSymbols.map(tpe.memberType)
      val typeTrees       : List[TypeTree] = typeReprs.map(Inferred.apply)
      val fieldNames      : List[String] = caseFieldSymbols.map(_.name)

      assert(fieldNames == typeNode.fieldNames) // 231219122236

      // ===========================================================================
      val constructorArgBlock: Term = {
        val companionTerm = tpe.typeSymbol.companionModule.termRef.pipe(Ref.term(_)) // TODO: is there a way not to transit via symbol?

        val selectApplyMethod = Select.unique(companionTerm, if (enumeratum) MethodNames.enumeratumWithName else MethodNames.caseClassApply)

        // ---------------------------------------------------------------------------
        val closureMethodSymbol = {
          val closureMethodType =
            MethodType(
              paramNames =
                  if (enumeratum) List(/* withName's */ "name")
                  else            fieldNames)(
                paramInfosExp = _ => typeReprs,
                resultTypeExp = _ => TypeRepr.of[Any/*T*/])

          Symbol.newMethod(
            parent = Symbol.spliceOwner,
            name   = MethodNames.arbitraryClosureName,
            tpe    = closureMethodType) }

        // ---------------------------------------------------------------------------
        val closureDefDef =
          DefDef(
            symbol = closureMethodSymbol,
            rhsFn  = { case List(terms: List[Term]) =>
              Some(selectApplyMethod.appliedToArgs(terms)) })

        val closure = Closure(
          meth = closureMethodSymbol.termRef.pipe(Ref.term(_)),
          tpe  = None)

        // ---------------------------------------------------------------------------
        Block(stats = List(closureDefDef), expr = closure) }

      // ===========================================================================
      // helpers

      def _helperSelect(name: String): Select =
          Select.unique('{HelperMethods}.asTerm, name)

        // ---------------------------------------------------------------------------
        def _helperApply(name: String)(terms: Term*): Apply =
          Apply(_helperSelect(name), terms.toList)

        // ---------------------------------------------------------------------------
        def _instanSelect(name: String): Select =
          Select.unique('{Instantiator}.asTerm, name)

      // ===========================================================================
      val fieldTerms: List[Term] =
        if (enumeratum) Nil
        else
          typeNode
            .leaf.fields
            .zipSameSize(caseFieldSymbols) /* see assert above (231219122236) */
            .toList
            .flatMap { (field, sym) =>
               if (!field.typeNode.isContainedDataClass) None
               else Some(field.key, field.typeNode, tpe.memberType(sym)) }
            .map { (key, subNode, fieldTpe) =>
              _helperApply(MethodNames.entry)(
                Literal(StringConstant(key)),
                subNode.containerTypeOpt match {
                  case None                 => rec(fieldTpe)(subNode)
                  case Some(Container._Opt) => rec(fieldTpe.typeArgs.force.one)                   (subNode.forceSoleTypeArg)
                  case Some(Container._Nes) => rec(fieldTpe.typeArgs.force.one)                   (subNode.forceSoleTypeArg)
                  case Some(Container._Pes) => rec(fieldTpe.typeArgs.force.one.typeArgs.force.one)(subNode.forceSoleTypeArg.forceSoleTypeArg) }) }

      // ---------------------------------------------------------------------------
      val applyFrom: Term =

        if (enumeratum)
          Apply(
            TypeApply(
              _instanSelect(MethodNames.instanEnumeratumWithName),
              typeTrees),
            List(constructorArgBlock))

        // ---------------------------------------------------------------------------
        else {
          val mapApply =
            _helperApply(MethodNames.entriesMap)(
              Apply(_helperSelect(MethodNames.entries(fieldTerms.size)), fieldTerms) )

          // ---------------------------------------------------------------------------
          Apply(
            TypeApply(
              _instanSelect(MethodNames.instanFrom(fieldNames.size)),
              typeTrees),
            List(constructorArgBlock, mapApply)) }

      // ===========================================================================
      /*
        ~translates to eg:

           gallia.reflect.Instantiator.from2[scala.Predef.String, scala.Predef.String](
             ((a: scala.Predef.String, A: scala.Predef.String) => galliatesting.TestMeta.Foo.apply(a, A)),
             gallia.reflect.macros3.InstantiatorCreatorMacro3.inline$HelperMethods.entriesMap(
               gallia.reflect.macros3.InstantiatorCreatorMacro3.inline$HelperMethods.entries0()))
      */

      (applyFrom: Term)
    }
  }
}

// ===========================================================================

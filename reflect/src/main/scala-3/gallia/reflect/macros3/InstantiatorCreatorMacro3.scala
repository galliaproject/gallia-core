package gallia
package reflect
package macros3

import aptus.Seq_

// ===========================================================================
private object InstantiatorCreatorMacro3 {
  import scala.quoted.{Quotes, quotes, Type, Expr, Varargs}

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
    // to save a bit of TypeApply-ing
    def entry(key: String, value: Instantiator): E = Tuple2[String, Instantiator](key, value)
    def entriesMap(values: Seq[E]): Map[String, Instantiator] = values.force.map }

  // ===========================================================================
  def rec(using q: Quotes)(tpe: q.reflect.TypeRepr)(typeNode: TypeNode): q.reflect.Term = {
    import quotes.reflect.*

         if !typeNode.isContainedDataClass && !typeNode.isContainedEnumeratum then '{Instantiator.Placeholder /* easier for now */}.asTerm
    else if  typeNode.isOptionOfSeq then rec(tpe.typeArgs.force.one.typeArgs.force.one)(typeNode.forceSoleTypeArg.forceSoleTypeArg)
    else if  typeNode.isOption      then rec(tpe.typeArgs.force.one)                   (typeNode.forceSoleTypeArg)
    else if  typeNode.        isSeq then rec(tpe.typeArgs.force.one)                   (typeNode.forceSoleTypeArg)
    else {
      val enumeratum = typeNode.leaf.isEnumeratum

      val caseFieldSymbols: List[Symbol]   = tpe.typeSymbol.caseFields
      val typeReprs       : List[TypeRepr] = if enumeratum then List(TypeRepr.of[String]) else caseFieldSymbols.map(tpe.memberType)
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
                  if enumeratum then List(/* withName's */ "name")
                  else               fieldNames)(
                paramInfosExp = _ => typeReprs,
                resultTypeExp = _ => TypeRepr.of[Any])

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
        if enumeratum then Nil
        else
          typeNode
            .leaf.fields
            .zipSameSize(caseFieldSymbols) /* see assert above (231219122236) */
            .toList
            .flatMap { (field, sym) =>
               if !field.typeNode.isContainedDataClass then None
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

        if enumeratum then
          Apply(
            TypeApply(
              _instanSelect(MethodNames.instanEnumeratumWithName),
              typeTrees),
            List(constructorArgBlock))

        // ---------------------------------------------------------------------------
        else {
          val varargs = // TODO: 240101204835 - populate map directly
            Varargs[Tuple2[String, Instantiator]](
              fieldTerms.map(_.asExprOf[Tuple2[String, Instantiator]]))

          val mapApply =
            _helperApply(MethodNames.entriesMap)(
              varargs.asTerm )

          // ---------------------------------------------------------------------------
          Apply(
            TypeApply(
              _instanSelect(MethodNames.instanFrom(fieldNames.size)),
              typeTrees),
            List(constructorArgBlock, mapApply)) }

      // ---------------------------------------------------------------------------
      (applyFrom: Term) } } }

// ===========================================================================

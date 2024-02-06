package gallia
package oswo
package source

// ===========================================================================
object SourceFluentBuilders {
  // TODO: t240202123805 - find simple macro-based smart constructor lib (exists?); just for fluency, not soundness

  implicit class OswoUnit_(u: Unit) {
    def methodDefinition(_name: String)   : Method = method(_Name(_name))
    def method(_name: _Name): Method = new Method(_name)

     class Method(_name: _Name) {
      def returnAny                      : ReturnType = new ReturnType("Any")
      def returnType(_returnType: String): ReturnType = new ReturnType(_returnType)
       class ReturnType(_returnType: String) {
        def body(_body: SourceCode): _MethodDefinition = body(_body.format)
        def body(_body: String) : _MethodDefinition =
          _MethodDefinition(_name, params = Nil, _returnType, _body) } }

    // ---------------------------------------------------------------------------
    def applyCallOn(_target: String): MethodCall = methodCall(_target + ".apply")
    def methodCall(_name: String)   : MethodCall = methodCall(_Name(_name))
    def methodCall(_name: _Name)    : MethodCall = new MethodCall(_name)

     class MethodCall(_name: _Name) {
      def args(_args: Seq[String]): _MethodCall =
        _MethodCall(_name, _args) }


    // ---------------------------------------------------------------------------
    def argAssignment(lhs: String) = new Lhs(lhs)
     class Lhs(lhs: String) {
       def eq(rhs: String): _ArgAssignment =
        _ArgAssignment(
          _Name(lhs), _Name(rhs)) }
  }

}

// ===========================================================================
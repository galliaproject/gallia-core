package gallia
package reflect

// ===========================================================================
case class Field(key: String, typeNode: TypeNode) {
    def formatDebug: String = s"${key}: ${typeNode.formatDebug}"
    def formatDefault: String = toString }

  // ---------------------------------------------------------------------------
  object Field {
    def string (name: String) = Field(name, TypeNodeBuiltIns.ScalaString)
    def int    (name: String) = Field(name, TypeNodeBuiltIns.ScalaInt)
    def double (name: String) = Field(name, TypeNodeBuiltIns.ScalaDouble)
    def boolean(name: String) = Field(name, TypeNodeBuiltIns.ScalaBoolean) }

// ===========================================================================

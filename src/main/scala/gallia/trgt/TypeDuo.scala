package gallia
package trgt

// ===========================================================================
class TypeDuo private ( // still a poor name, but better than "HT"
          val typeNode       :        TypeNode,
          val instantiatorOpt: Option[Instantiator])
        extends Serializable /* TODO: t240108113850 - ideally shouldn't be needed, but see the likes of 240108113936 */
          with HasType {
      final override def typeDuo: TypeDuo = this }

    // ===========================================================================
    private[gallia] object TypeDuo {
      def build[T: WTT]: TypeDuo = implicitly[WTT[T]].pipe { wtt => new TypeDuo(wtt.typeNode, wtt.instantiatorOpt) }

      // ---------------------------------------------------------------------------
      def fromHasType     (value: HasType) : TypeDuo = new TypeDuo(value.typeNode, value.instantiatorOpt)
      def fromTypeNodeOnly(value: TypeNode): TypeDuo = new TypeDuo(value,          None) }

  // ===========================================================================
  class TypeDuo2(val ht1: TypeDuo, val ht2: TypeDuo) extends HasTypes2

    // ---------------------------------------------------------------------------
    object TypeDuo2 {
      def from[T1: WTT, T2: WTT] =
        new TypeDuo2(TypeDuo.build[T1], TypeDuo.build[T2]) }

// ===========================================================================

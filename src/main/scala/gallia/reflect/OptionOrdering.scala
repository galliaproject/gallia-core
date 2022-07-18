package gallia
package reflect

// ===========================================================================
object OptionOrdering {

  def optionAF[T](ord: Ordering[T]): Ordering[Option[T]] = new OptionOrderingAF[T] { val optionOrdering = ord }
  def optionAL[T](ord: Ordering[T]): Ordering[Option[T]] = new OptionOrderingAL[T] { val optionOrdering = ord }

  def optionDF[T](ord: Ordering[T]): Ordering[Option[T]] = optionAF[T](ord)
  def optionDL[T](ord: Ordering[T]): Ordering[Option[T]] = optionAL[T](ord)

  // ===========================================================================
  private trait OptionOrderingAF[T] extends Ordering[Option[T]] { // same as Ordering.Option
      def optionOrdering: Ordering[T]
      def compare(x: Option[T], y: Option[T]) = (x, y) match {
        case (None, None)       =>  0
        case (None, _)          => -1
        case (_, None)          =>  1
        case (Some(x), Some(y)) => optionOrdering.compare(x, y) } }

    // ---------------------------------------------------------------------------
    private trait OptionOrderingAL[T] extends Ordering[Option[T]] {
      def optionOrdering: Ordering[T]
      def compare(x: Option[T], y: Option[T]) = (x, y) match {
        case (None, None)       =>  0
        case (None, _)          =>  1                            // swapped
        case (_, None)          => -1
        case (Some(x), Some(y)) => optionOrdering.compare(x, y) } }

    // ---------------------------------------------------------------------------
    private trait OptionOrderingDF[T] extends Ordering[Option[T]] {
      def optionOrdering: Ordering[T]
      def compare(x: Option[T], y: Option[T]) = (y, x) match {   // x <-> y
        case (None, None)       =>  0
        case (None, _)          => -1
        case (_, None)          =>  1
        case (Some(x), Some(y)) => optionOrdering.compare(x, y) } }

    // ---------------------------------------------------------------------------
    private trait OptionOrderingDL[T] extends Ordering[Option[T]] {
      def optionOrdering: Ordering[T]
      def compare(x: Option[T], y: Option[T]) = (y, x) match {   // x <-> y
        case (None, None)       =>  0
        case (None, _)          =>  1                            // swapped
        case (_, None)          => -1
        case (Some(x), Some(y)) => optionOrdering.compare(x, y) } }

}

// ===========================================================================

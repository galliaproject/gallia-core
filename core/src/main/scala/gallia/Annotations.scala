package gallia

import scala.annotation.StaticAnnotation

// ===========================================================================
trait Annotations {
  /** TODO: t210115151942 - to help find where types are being pattern-matched easily (think of this as a stable comment) */
  private[gallia] class TypeMatching  (val message: String = "") extends StaticAnnotation

  /** Intentionally leaving out some */
  private[gallia] class PartialTypeMatching  (val message: String = "") extends StaticAnnotation

  /** TODO: t201017102332 - so can locate places where only Numbers need to be properly abstracted (especiall integer vs real) */
  private[gallia] class NumberAbstraction(val message: String = "") extends StaticAnnotation

  /** TODO: t201209095425 - so can locate places with Int is used for size (as opposed to Long in Spark for instance) */
  private[gallia] class IntSize       (val message: String = "") extends StaticAnnotation

  /** TODO: t210123183101 - to help find where distributivity might be an issue (eg .head does not make sense unless sorted first) */
  private[gallia] class Distributivity(val message: String = "") extends StaticAnnotation

  /** TODO: t210123183102 - to help find where scalability might be an issue */
  private[gallia] class Scalability   (val message: String = "") extends StaticAnnotation

  /** TODO: t210127173933 - to keep track of restriction, rationale being that if you need more than N, either you can use an intermediate state or you may just be doing something wrong */
  private[gallia] class Max3          (val message: String = "") extends StaticAnnotation
  private[gallia] class Max5          (val message: String = "") extends StaticAnnotation
  private[gallia] class Max8          (val message: String = "") extends StaticAnnotation
  private[gallia] class Max10         (val message: String = "") extends StaticAnnotation
}

// ===========================================================================

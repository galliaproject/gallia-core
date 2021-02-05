package aptus.utils

import aptus.String_
import java.math.MathContext
import java.text.NumberFormat
import java.util.Locale

// ===========================================================================
object NumberUtils {

  lazy val IntegerFormatter = NumberFormat.getIntegerInstance(Locale.US)
  lazy val NumberFormatter  = NumberFormat.getNumberInstance (Locale.US)

  // ---------------------------------------------------------------------------
  private def mathContext(setPrecision: Int): MathContext = MathContexts(setPrecision)

    private lazy val MathContexts = // TODO: smarter caching (q: any built-in?)
      Map(
           1 -> new MathContext( 1),
           2 -> new MathContext( 2),
           3 -> new MathContext( 3),
           4 -> new MathContext( 4),
           5 -> new MathContext( 5),
           6 -> new MathContext( 6),
           7 -> new MathContext( 7),
           8 -> new MathContext( 8),
           9 -> new MathContext( 9),
          10 -> new MathContext(10),
          11 -> new MathContext(11),
          12 -> new MathContext(12),
          13 -> new MathContext(13),
          14 -> new MathContext(14),
          15 -> new MathContext(15),
          16 -> new MathContext(16) )

  // ===========================================================================
  def significantFigures(nmbr: Double, setPrecision: Int): Double =
    if (nmbr.isNaN || nmbr.isInfinity) nmbr
    else
      BigDecimal(nmbr) // TODO: *has* to transit through big?
        .round(mathContext(setPrecision))
        .doubleValue()

  // ===========================================================================
  def isValidInt(str: String): Boolean =
    !str.isEmpty &&
     // FIXME: sign and check size? sigh java...
     // TODO: else use guava's tryDouble/... and so on... (see spark version issues though)
     (if (str.head == '-') str.tail.isDigits
      else                 str     .isDigits)

}

// ===========================================================================

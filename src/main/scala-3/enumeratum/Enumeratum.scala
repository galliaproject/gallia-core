package enumeratum

trait Enum[T]   { def findValues: Seq[T] = throw new IllegalStateException }
trait EnumEntry { def entryName: String  = throw new IllegalStateException }


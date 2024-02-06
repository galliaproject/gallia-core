package gallia
package oswo

// ===========================================================================
object OswoSerDes {

  def serializeFunction[T](t: T) = {
    val baos = new java.io.ByteArrayOutputStream()

    val oos = new java.io.ObjectOutputStream(baos)
    oos.writeObject(t)
    oos.close

    val bytes = baos.toByteArray()
    baos.close() // still needed?

    bytes }

  // ---------------------------------------------------------------------------
  def deserializeFunction[T](bytes: Array[Byte]): T = {
    val bais = new java.io.ByteArrayInputStream(bytes)

    val ois   = new java.io.ObjectInputStream(bais)
    val value = ois.readObject.asInstanceOf[T]

    ois .close()
    bais.close()

    value }

}

// ===========================================================================

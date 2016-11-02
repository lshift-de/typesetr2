package net.lshift.typesetr.pandoc

/**
 * Typesetr-specific document fragments use a simple counter
 * to uniquely define their names in a consistent way.
 *
 * The instance of `UUIDGen` is simply passed around as a token.
 */
class UUIDGen {
  private[this] var counter = 0

  def increment(): Int = {
    val res = counter
    counter = counter + 1
    res
  }
}

object UUIDGen {
  def apply(): UUIDGen = new UUIDGen()
}

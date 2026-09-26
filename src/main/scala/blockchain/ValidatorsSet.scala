package blockchain

import java.security.PublicKey

/** V = (v_1, ..., v_N), the same enumeration at every replica. The order only
  * lists the entries of a certificate; membership is an O(1) lookup.
  */
final case class ValidatorsSet(members: Vector[PublicKey]):
  private val index = members.toSet
  require(index.size == members.size, "validators must be distinct")

  def size: Int = members.size
  def contains(v: PublicKey): Boolean = index(v)

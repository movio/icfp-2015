/**
 * NOT THREAD SAFE
 */
class Random(seed: Long) {
  var last: Long = seed

  /**
   * overflow is fine because mod 2^32
   */
  def next(): Int = {
    var t: Int = last.asInstanceOf[Int] // mod 32
    val output = (t >> 16) & 0x7fff
    t = ((t * 1103515245) + 12345)
    last = t
    //println(last)
    if (last < 0) last += 4294967296L
    output
  }
}

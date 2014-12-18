package scalax.util

object MD5 {
  def hash(s: String) = {
    val m = java.security.MessageDigest.getInstance("MD5")
    val b = s.getBytes("UTF-8")
    m.update(b, 0, b.length)
    new java.math.BigInteger(1, m.digest()).toString(16)
  }
  
  def hash(s: String*) = {
    val m = java.security.MessageDigest.getInstance("MD5")
    for (st <- s) {
    	val b = st.getBytes("UTF-8")
    	m.update(b, 0, b.length)
    }
    new java.math.BigInteger(1, m.digest()).toString(16)
  }
  
  def hashAny(a: Any*) = {
    val m = java.security.MessageDigest.getInstance("MD5")
    for (e <- a) {
    	val s = Integer.toHexString(System.identityHashCode(e))
    	val b = s.getBytes("UTF-8")
    	m.update(b, 0, b.length)
    }
    new java.math.BigInteger(1, m.digest()).toString(16)
  }
}
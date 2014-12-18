package scalax.util

trait Visitor[T, K, L] {
  def visit(e:T, a:L):K
}

package scalax.util
trait Visitable[T] {
  def visit[K, L](v:Visitor[T, K, L], a:L):K
}

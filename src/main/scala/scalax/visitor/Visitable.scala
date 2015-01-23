package scalax.visitor

trait Visitable[T] {
  def visit[K, L](v:Visitor[T, K, L], a:L):K
  
  def visit[K, L](a:L)(implicit v:Visitor[T, K, L]):K
}

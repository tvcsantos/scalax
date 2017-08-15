package scalax.visitor

trait Visitor[T, K, L] {
  def visit(e:T, a:L):K
}

class VisitorException(msg:String) extends Exception(msg)

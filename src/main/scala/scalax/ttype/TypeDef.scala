package scalax.ttype

import scala.util.parsing.input.Positional
import scalax.exception.ScalaException
import scalax.visitor.{NameNotFound, TypeFreeNames}

case class TypeDef[T <: Type](name:String, t:T) extends Positional {
  override def toString = s"type $name = $t"
}

object TypeDef {
  import scalax.util.{Environment, TypeVariable}
  
  def checkTypes[T <: Type](env:Environment[String, TypeDef[T]], l:T*)
  	(implicit ftyn:TypeFreeNames[T]):Unit = {
    if (l.isEmpty) ()
    else {
      val x = l.head
      val xs = l.tail
      ftyn.visit(x, ()).foreach(e => e match {
        case y: TypeVariable =>
          env.find(y.getName) match {
            case None =>
              val msg = s"unknown type ${y.getName}"
              throw new NameNotFound(y.getName, 
                  ScalaException.getPositionalMessage(y, msg))
            case _ => ()
          }
        case _ => ()
      })
      checkTypes(env, xs:_*)
    }
  }
   
  def checkType[T <: Type](env:Environment[String, TypeDef[T]], t:T)
  	(implicit ftyn:TypeFreeNames[T]) = checkTypes(env, t)
  	  
  def checkTypeDefs[T <: Type](env:Environment[String, TypeDef[T]], 
      l:TypeDef[T]*)(implicit ftyn:TypeFreeNames[T]) =
        checkTypes(env, l.map(_.t):_*)
  
  def checkTypeDef[T <: Type](env:Environment[String, TypeDef[T]], 
      t:TypeDef[T])(implicit ftyn:TypeFreeNames[T]) =
        checkTypeDefs(env, t)
}
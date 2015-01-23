package scalax.visitor

import scalax.util.Environment
import scalax.util.Visitor
import scalax.util.Variable

/*trait GeneralTypeChecker[T, U <: Type, K, L] extends 
	Visitor[T, T, (Environment[K], L)]*/

trait TypeExpander[U <: Type]  extends
  Visitor[U, U, Environment[String, TypeDef[U]]]

trait TypeChecker[T, U <: Type, L] extends 
	//GeneralTypeChecker[T, U, U, L]
	Visitor[T, T, (Environment[String, U], L)] {

  def getTypeExpander():TypeExpander[U]

  final def expandTypeOpt(t:Option[U],
    env:Environment[String, TypeDef[U]]):Option[U] = {
    t match {
      case None => None
      case Some(ty) => Some(getTypeExpander() visit(ty, env))
    }
  }

  final def expandType(t:U, env:Environment[String, TypeDef[U]]):U = {
    //TypeDef.checkType[U](env, t)(tyFn)
    getTypeExpander() visit(t, env)
  }

}

/*trait ParametricTypeChecker[T, U <: Type, L] extends 
	GeneralTypeChecker[T, U, Set[U], L]
	//Visitor[T, T, (Environment[Set[U]], L)]*/

class TypeCheckException(msg:String) extends Exception(msg)

class WrongType(msg:String) extends TypeCheckException(msg)
 
class NameNotFound(name:String, msg:String) extends TypeCheckException(msg) {
  def getName():String = name
}

/*class TypeNotFound[T <: Type](t:T, msg:String) 
	extends TypeCheckException(msg) {
  def getType():T = t  
}*/
  
class WrongArgumentsNumber(msg:String) extends TypeCheckException(msg)

trait SubstitutionVisitor[T, U] extends Visitor[T, T, (String, U)]

trait SubstMapVisitor[T, R, V, U] extends Visitor[T, R, Map[V, U]]

final class NamesVisitor[T](fnv:FreeNamesVisitor[T], bnv:BoundNamesVisitor[T]) 
	extends Visitor[T, Set[String], Unit] {
  def visit(e:T, a:Unit = ()):Set[String] =
    (fnv visit(e)) ++ (bnv visit(e))
}

trait FreeNamesVisitor[T] extends Visitor[T, Set[String], Unit] {
  def visit(e:T, a:Unit = ()):Set[String]
}

trait BoundNamesVisitor[T] extends Visitor[T, Set[String], Unit] {
  def visit(e:T, a:Unit = ()):Set[String]
}

class VariablesVisitor[T, U <: Variable](fnv:FreeVariablesVisitor[T, U], 
    bnv:BoundVariablesVisitor[T, U]) extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U] =
    (fnv visit(e)) ++ (bnv visit(e))
}

trait FreeVariablesVisitor[T, U <: Variable] extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U]
}

trait BoundVariablesVisitor[T, U <: Variable] extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U]
}

trait FreeTypesVisitor[T, U <: Type] extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U]
}

trait BoundTypesVisitor[T, U <: Type] extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U]
}

class TypesVisitor[T, U <: Type](fnv:FreeTypesVisitor[T, U], 
    bnv:BoundTypesVisitor[T, U]) extends Visitor[T, Set[U], Unit] {
  def visit(e:T, a:Unit = ()):Set[U] =
    (fnv visit(e)) ++ (bnv visit(e))
}

trait Simplifier[T] extends Visitor[T, T, Unit] {
  def visit(e:T, a:Unit = ()):T
}

trait BoundNamesListVisitor[T] extends Visitor[T, List[String], Unit] {
  def visit(e:T, a:Unit = ()):List[String]
}

object Names {
  def getBoundNameClashes[T](as: T*)
  	(implicit bnlv:BoundNamesListVisitor[T], nv:NamesVisitor[T],
  	    fnv:FreeNamesVisitor[T]):Set[String] = {
    val bnl = as.flatMap(bnlv.visit(_))
    val ns = as.flatMap(nv.visit(_)).toSet
    val fns = as.flatMap(fnv.visit(_)).toSet
    (bnl diff(ns.toSeq)).toSet ++ (fns & bnl.toSet)
  }
  
  def hasBoundNameClashes[T](as: T*)
  	(implicit bnlv:BoundNamesListVisitor[T], nv:NamesVisitor[T],
  	    fnv:FreeNamesVisitor[T]):Boolean =
    !(getBoundNameClashes(as:_*) isEmpty)
}

trait TypeFreeNames[T <: Type] extends Visitor[T, Set[T], Unit]


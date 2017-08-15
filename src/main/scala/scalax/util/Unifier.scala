package scalax.util

/**
 * Artificial Intelligence A Modern Approach (3rd Edition): 
 * Figure 9.1, page 328.
 * 
 * <pre>
 * function UNIFY(x, y, theta) returns a substitution to make x and y identical
 *   inputs: x, a variable, constant, list, or compound
 *           y, a variable, constant, list, or compound
 *           theta, the substitution built up so far 
 *           	(optional, defaults to empty)
 *           
 *   if theta = failure then return failure
 *   else if x = y the return theta
 *   else if VARIABLE?(x) then return UNIVY-VAR(x, y, theta)
 *   else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
 *   else if COMPOUND?(x) and COMPOUND?(y) then
 *       return UNIFY(x.ARGS, y.ARGS, UNIFY(x.OP, y.OP, theta))
 *   else if LIST?(x) and LIST?(y) then
 *       return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
 *   else return failure
 *   
 * ----------------------------------------------------------------------------
 * 
 * function UNIFY-VAR(var, x, theta) returns a substitution
 *            
 *   if {var/val} E theta then return UNIFY(val, x, theta)
 *   else if {x/val} E theta then return UNIFY(var, val, theta)
 *   else if OCCUR-CHECK?(var, x) then return failure
 *   else return add {var/x} to theta
 * </pre>
 * 
 * Figure 9.1 The unification algorithm. The algorithm works by comparing the
 * structures of the inputs, elements by element. The substitution theta that 
 * is the argument to UNIFY is built up along the way and is used to make sure 
 * that later comparisons are consistent with bindings that were established 
 * earlier. In a compound expression, such as F(A, B), the OP field picks out 
 * the function symbol F and the ARGS field picks out the argument list (A, B).
 */

/**
 * @author Tiago Santos (Scala Version)
 * @author Ravi Mohan
 * @author Ciaran O'Reilly
 * 
 */

import scala.reflect.ClassTag
import scala.collection.mutable.{LinkedHashMap => MutableLinkedHashMap}
import scala.collection.mutable.{Map => MutableMap}
import scalax.visitor.SubstMapVisitor

//Subst = Map[I, U]
//I = VARIABLE
//U = TYPE OF SUBSTITUTION RESULT 
abstract class Unifier[I <: Variable : ClassTag]//, U : ClassTag]
		(substVisitor:SubstMapVisitor[Any, Any, I, Any]/*[U, U, I, U]*/) {
  
  import scala.collection.mutable.{
    Map => MutableMap,
    LinkedHashMap => MutableLinkedHashMap
  }

  def unify[T](x:T, y:T):Option[Map[I, Any/*U*/]] = {
    val r = unify(x, y, Some(MutableLinkedHashMap[I, Any/*U*/]())) 
    r match {
      case None => None
      case Some(m) => Some(m.toMap)
    }
  }

  /**
   * <code>
   * function UNIFY(x, y, theta) returns a substitution to make x and y 
   * identical
   *   inputs: x, a variable, constant, list, or compound
   *           y, a variable, constant, list, or compound
   *           theta, the substitution built up so far 
   *           	(optional, defaults to empty)
   * </code>
   * 
   * @return a Map<Variable, Term> representing the substitution (i.e. a set
   *         of variable/term pairs) or null which is used to indicate a
   *         failure to unify.
   */
  protected def unify(x:Any, y:Any, 
      theta:Option[MutableMap[I, Any/*U*/]]):
	  Option[MutableMap[I, Any/*U*/]] = {
    val iclazz = implicitly[ClassTag[I]].runtimeClass
    //val uclazz = implicitly[ClassTag[U]].runtimeClass
	// if theta = failure then return failure
	if (theta == None) {
	  None
	} else if (x == y) {
	// else if x = y then return theta
	  theta
	} else if (iclazz.isInstance(x)/* || isVariable(x)*/) {
	// else if VARIABLE?(x) then return UNIVY-VAR(x, y, theta)
	  unifyVar(x.asInstanceOf[I], y, theta.get)
	} else if (iclazz.isInstance(y)/* || isVariable(y)*/) {
	// else if VARIABLE?(y) then return UNIFY-VAR(y, x, theta)
	  unifyVar(y.asInstanceOf[I], x, theta.get)
	} else if (isCompound(x) && isCompound(y)) {
	// else if COMPOUND?(x) and COMPOUND?(y) then
	  // return UNIFY(x.ARGS, y.ARGS, UNIFY(x.OP, y.OP, theta))
	  val r = unify(args(x), args(y), unifyOps(op(x), op(y), theta))
	  r
	} else {
	// else return failure
	  None
	}
  }
  
  //protected def isVariable(x:Any):Boolean

  // else if LIST?(x) and LIST?(y) then
  // return UNIFY(x.REST, y.REST, UNIFY(x.FIRST, y.FIRST, theta))
  protected def unify(x:List[Any], y:List[Any], 
      theta:Option[MutableMap[I, Any/*U*/]]):Option[MutableMap[I, Any/*U*/]] = {
    if (theta == None) {
      None
    } else if (x.size != y.size) {
      None
    } else {
      (x, y) match {
        case (Nil, Nil) => theta
		case (ex::Nil, ey::Nil) => unify(ex, ey, theta)
		case (ex::xs, ey::ys) => unify(xs, ys, unify(ex, ey, theta))
      }
    }
  }

  //
  // PROTECTED METHODS
  //

  // Note: You can subclass and override this method in order
  // to re-implement the OCCUR-CHECK?() to always
  // return false if you want that to be the default
  // behavior, as is the case with Prolog.
  // Note: Implementation is based on unify-bug.pdf document by Peter Norvig:
  // http://norvig.com/unify-bug.pdf
  protected def occurCheck(
      theta:MutableMap[I, Any/*U*/], vvar:I, x:Any):Boolean = {
    val iclazz = implicitly[ClassTag[I]].runtimeClass
    // ((equal var x) t)
    if (vvar == x) {
      true
	// ((bound? x subst)if 
	} else if (iclazz.isInstance(x) && theta.contains(x.asInstanceOf[I])) {
	  //  (occurs-in? var (lookup x subst) subst))
	  occurCheck(theta, vvar, theta.get(x.asInstanceOf[I]).get)
	// ((consp x) (or (occurs-in? var (first x) subst) 
	//                                   (occurs-in? var (rest x) subst)))
	} else if (isFunction(x)) {
		// (or (occurs-in? var (first x) subst) 
		//                               (occurs-in? var (rest x) subst)))
	  for (fxt <- args(x)) {
	    if (occurCheck(theta, vvar, fxt)) {
	      return true
		}
	  }
	}
	false
  }
  
  protected def isFunction(x:Any):Boolean

  //
  // PRIVATE METHODS
  //

  /**
   * <code>
   * function UNIFY-VAR(var, x, theta) returns a substitution
   *   inputs: var, a variable
   *       x, any expression
   *       theta, the substitution built up so far
   * </code>
   */
  protected def unifyVar(vvar:I, 
      x:Any, theta:MutableMap[I, Any/*U*/]):Option[MutableMap[I, Any/*U*/]] = {
    val iclazz = implicitly[ClassTag[I]].runtimeClass
	//val uclazz = implicitly[ClassTag[U]].runtimeClass
	if (!x.isInstanceOf[Any]/*uclazz.isInstance(x)*/) {
	  None
	} else if (theta.keySet.contains(vvar)) {
	  // if {var/val} E theta then return UNIFY(val, x, theta)
	  unify(theta.get(vvar).get, x, Some(theta))
	} else if (iclazz.isInstance(x) && 
	    theta.keySet.contains(x.asInstanceOf[I])) {
	  // else if {x/val} E theta then return UNIFY(var, val, theta)
	  unify(vvar, theta.get(x.asInstanceOf[I]).get, Some(theta))
	} else if (occurCheck(theta, vvar, x)) {
	  // else if OCCUR-CHECK?(var, x) then return failure
	  None
	} else {
	  // else return add {var/x} to theta
	  cascadeSubstitution(theta, vvar, x.asInstanceOf[Any/*U*/])
	  Some(theta)
	}
  }

  protected def unifyOps(x:String, y:String,
      theta:Option[MutableMap[I, Any/*U*/]]):Option[MutableMap[I, Any/*U*/]] = {
    if (theta == None) {
      None
    } else if (x == y) {
      theta
    } else {
      None
    }
  }

  protected def args(x:Any):List[Any]

  protected def op(x:Any):String

  protected def isCompound(x:Any):Boolean

  // See:
  // http://logic.stanford.edu/classes/cs157/2008/miscellaneous/faq.html#jump165
  // for need for this.
  protected def cascadeSubstitution(theta:MutableMap[I, Any/*U*/], vvar:I, x:Any/*U*/):
	  MutableMap[I, Any/*U*/] = {
    theta.put(vvar, x);
    for (v <- theta.keySet) {
      theta.put(v, substVisitor.visit(theta.get(v).get, theta.toMap))
      //theta.put(v, _substVisitor.subst(theta, theta.get(v)));
    }
    // Ensure Function Terms are correctly updates by passing over them again
	// Fix for testBadCascadeSubstitution_LCL418_1()
	for (v <- theta.keySet) {	  
	  val t = theta.get(v).get
	  if (isFunction(t)) {
	    theta.put(v, substVisitor.visit(t, theta.toMap));
	  }
	}
	theta
  }
}
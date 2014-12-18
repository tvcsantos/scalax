/* ==========================================
 * JGraphT : a free Java graph-theory library
 * ==========================================
 *
 * Project Info:  http://jgrapht.sourceforge.net/
 * Project Creator:  Barak Naveh (http://sourceforge.net/users/barak_naveh)
 *
 * (C) Copyright 2003-2010, by Barak Naveh and Contributors.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation; either version 2.1 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
 * or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation,
 * Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307, USA.
 */
/* -------------------------
 * UnionFind.java
 * -------------------------
 * (C) Copyright 2010-2010, by Tom Conerly and Contributors.
 *
 * Original Author:  Tom Conerly
 * Contributor(s):
 *
 * Changes
 * -------
 * 02-Feb-2010 : Initial revision (TC);
 *
 */
package scalax.util

/**
 * An implementation of <a
 * href="http://en.wikipedia.org/wiki/Disjoint-set_data_structure">Union
 * Find</a> data structure. Union Find is a disjoint-set data structure. It
 * supports two operations: finding the set a specific element is in, and
 * merging two sets. The implementation uses union by rank and path compression
 * to achieve an amortized cost of O(a(n)) per operation where a is the inverse
 * Ackermann function. UnionFind uses the hashCode and equals method of the
 * elements it operates on.
 *
 * @author Tom Conerly
 * @since Feb 10, 2010
 */
class UnionFind[T] {
  //~ Instance fields --------------------------------------------------------

  import scala.collection.mutable.{
    Map => MutableMap,
    HashMap => MutableHashMap
  }

  private val parentMap: MutableMap[T, T] = MutableHashMap()
  private val rankMap: MutableMap[T, Int] = MutableHashMap()

  //~ Constructors -----------------------------------------------------------

  /**
   * Creates a UnionFind instance with all of the elements of elements in
   * seperate sets.
   */
  def this(elements: Set[T]) {
    this()
    for (element <- elements) {
      parentMap.put(element, element)
      rankMap.put(element, 0)
    }
  }

  //~ Methods ----------------------------------------------------------------

  /**
   * Adds a new element to the data structure in its own set.
   *
   * @param element The element to add.
   */
  def addElement(element: T): Unit = {
    parentMap.put(element, element)
    rankMap.put(element, 0)
  }

  /**
   * @return map from element to parent element
   */
  protected def getParentMap(): MutableMap[T, T] = {
    parentMap
  }

  /**
   * @return map from element to rank
   */
  protected def getRankMap(): MutableMap[T, Int] = {
    rankMap
  }

  /**
   * Returns the representative element of the set that element is in.
   *
   * @param element The element to find.
   *
   * @return The element representing the set the element is in.
   */
  def find(element: T): Option[T] = {
    val parentOpt = parentMap.get(element)

    parentOpt match {
      case None => None
      case Some(parent) =>
        if (parent.equals(element)) {
          Some(element)
        } else {
          val newParentOpt = find(parent)
          parentMap.put(element, newParentOpt.get)
          Some(newParentOpt.get)
        }
    }
  }

  /**
   * Merges the sets which contain element1 and element2.
   *
   * @param element1 The first element to union.
   * @param element2 The second element to union.
   */
  def union(element1: T, element2: T): Boolean =
    {
      if (!parentMap.contains(element1)
        || !parentMap.contains(element2)) {
        return false
      }

      val parent1 = find(element1).get
      val parent2 = find(element2).get

      //check if the elements are already in the same set
      if (!parent1.equals(parent2)) {

        val rank1Opt = rankMap.get(parent1)
        val rank2Opt = rankMap.get(parent2)
        if (rank1Opt.get > rank2Opt.get) {
          parentMap.put(parent2, parent1)
        } else if (rank1Opt.get < rank2Opt.get) {
          parentMap.put(parent1, parent2)
        } else {
          parentMap.put(parent2, parent1)
          rankMap.put(parent1, rank1Opt.get + 1)
        }
      }

      true
    }
}

// End UnionFind.java

package org.sireum.amandroid.util

/**
 * @author <a href="mailto:fgwei@k-state.edu">Fengguo Wei</a>
 */
object CombinationIterator {
	private def combination[T](xx: List[List[T]], i : Int) : List[T] = xx match {
    case Nil => Nil
    case x :: xs => x(i % x.length) :: combination[T](xs, i / x.length)
  }                                              
  
  def combinationIterator[T](ll: List[List[T]]) : Iterator[List[T]] = {
    Iterator.from(0).takeWhile(n => n < ll.map(_.length).product).map(combination[T](ll,_))
  }
}
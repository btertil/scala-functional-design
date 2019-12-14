abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
  def union(other: IntSet): IntSet
}



class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

  //def this(elem: Int) = this(elem, new Empty, new Empty)

  override def contains(x: Int): Boolean = {
    if (x < elem) left contains x
    else if (x > elem) right contains x
    else true
  }


  override def include(x: Int): IntSet =  {
    if (elem < x) new NonEmpty(elem, left include x, right)
    else if (elem > x) new NonEmpty(elem, left, right include x)
    else this
  }

  override def toString: String = "{" + left + elem + right + "}"

  def union(other:  IntSet): IntSet = ((left union right) union other) include elem

}


object Empty extends IntSet {
  def contains(x: Int): Boolean = false
  def include(x: Int): IntSet = new NonEmpty(x, Empty, Empty)
  override def toString: String = "."

  def union(other: IntSet): IntSet = other
}

val t1 = new NonEmpty(5, Empty, Empty)
t1 contains 5
t1 contains 2

val t2 = t1 include 2
t2 contains 2

t1.toString
t2.toString

val t3 = t1 union (new NonEmpty(7, Empty, Empty) union new NonEmpty(12, Empty, Empty))
t3 union (new NonEmpty(elem = 2, Empty, Empty) union new NonEmpty(15, Empty, Empty))

t3.contains(7)

abstract class IntSet {
  def contains(x: Int): Boolean
  def include(x: Int): IntSet
}


class NonEmpty(elem: Int, left: IntSet, right: IntSet) extends IntSet {

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


}
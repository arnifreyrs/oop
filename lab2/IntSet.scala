class IntSet {
  private val MAX = 10000
  private val elems = new Array[Int](MAX)
  private var size = 0

  def isEmpty = size == 0

  def contains(x: Int) : Boolean = { find(x) >= 0 }

  private def find(x: Int): Int = {
    for (i <- 0 until size) {
      if (elems(i) == x) return i
    }
    -1
  }

  def insert(x: Int) {
    if (find(x) < 0) {
      assume(size < MAX)
      elems(size) = x
      size += 1
    }
  }

  def delete(x: Int) {
    val pos = find(x)
    if (pos >= 0) {
      elems(pos) = elems(size)
      size -= 1
    }
  }

  def exchange(x: Int, newX: Int): Unit ={
    if(find(x) >= 0 && find(newX) < 0){
      delete(x)
      insert(newX)
    }
  }

  def badExchange(x: Int, newX: Int)= {
    val pos = find(x)
    if(pos >= 0 && find(newX) < 0){
      elems(pos) = newX
    }
  }
}

class MaxIntSet(MAX: Int) extends IntSet {
  private var max = 0

  override def insert(x: Int): Unit = {
    super.insert(x)
    if(x > getMax) max = x
  }

  override def delete(x: Int): Unit = {
    super.delete(x)

    if(x == max){
      for (i<-max to 0){
        if(contains(i)){
          max = i
          return
        }
      }
    }
  }

  def getMax: Int = {
    max
  }
}


import scala.collection.immutable.Vector
import scala.collection.immutable.TreeSet

class conj(val it: TreeSet[Short]) extends Ordered[conj] {

  def compare(b: conj): Int = {

    def doit(b: conj): Int = {
      val sz = -b.it.size + it.size

      def dothem(x: List[Int]): Int = {
        if (x.isEmpty) 0 else if (x.head < 0) -1 else if (x.head > 0) 1
        else dothem(x.tail);
      }

      if (sz < 0) -1 else if (sz > 0) 1 else {
        val them = for ((m, a) <- (it zip b.it).toList) yield {
          m - a
        }
        dothem(them);
      }
    }

    val res = doit(b)
    //  println("Comparing " + this + " and " + b +" -> "+res);

    res

  }
  /* 
  override def equals(o: Any) = o match {
    
    
    case that: conj => compare(that)==0
    case _ => false
  }*/

  def *(v: Short) = {
    new conj(it + v)
  }

  def +(ot: conj) = {
    new xor(new TreeSet[conj]) + this + ot;
  }

  override def toString = {

    if (it.isEmpty) "T" else {

      var res = "";
      var has = false;
      for (s <- it) {
        if (has) res = res + ".";
        res = res + s.toHexString
        has = true;
      }
      res

    }
  }

}

class xor(val it: TreeSet[conj]) extends Ordered[xor] {

  def compare(b: xor): Int = {
    -1
  }

  def +(v: conj) = {
    if (it.contains(v)) new xor(it - v) else {
      new xor(it + v)
    }

  }

  override def toString = {
    if (it.isEmpty) "F" else {

      var res = "";
      var has = false;
      for (s <- it) {
        if (has) res = res + " + ";
        res = res + s
        has = true;
      }
      res
    }
  }
}

class equation {
  val it = TreeSet[xor]();
}

object T {
  def apply(vs: Short*): conj = {
    var x = new conj(new TreeSet);

    for (v <- vs) {
      x = x * v
    }
    x
  }
}
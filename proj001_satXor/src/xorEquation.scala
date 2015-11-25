

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

  def *(v: conj) = {

    new conj(it ++ v.it)
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

  def +(v: conj): xor = {
    if (it.contains(v)) new xor(it - v) else {
      new xor(it + v)
    }

  }

  def +(v: xor): xor = {
    var res: xor = v
    for (i: conj <- it) {
      res = res + i
    }
    res

  }

  def **(v: xor) = {

    var res = new xor(new TreeSet[conj]);
    for (c <- it.toList) {
      for (cv <- v.it) {
        res = res + (c * cv)
      }
    }
    res
  }

  def **(cv: conj) = {

    var res = new xor(new TreeSet[conj]);
    for (c <- it.toList) {
      res = res + (c * cv)
    }
    res
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


import scala.annotation.tailrec
object T {

  implicit def conjToXor(v: conj): xor = {
    var x = new xor(new TreeSet) + v;

    x
  }

  def diag(i: Int, size: Int): xor = {

    @tailrec def recdi(x : Int, y : Int, curr : List[Tuple2[Int,Int]] ) : List[Tuple2[Int,Int]]={
      if(y<0 || x>= size/2){
        curr
      }else
      {
        recdi(x+1,y-1,(x,y)::curr)
      }
      
      
    }
    
    def subdi(x: Int, y: Int): xor = {
      var r = new xor(new TreeSet);
      
      for((x,y)<- recdi(x,y, List() )){
        r= r+T(x,y+size/2)
      }

      r
    }

    if(i >= size -1){
      T()
    }else
    if (i < size / 2) {
      subdi(0, i)
    } else {
      subdi(i - size / 2 +1, size / 2-1)
    }

  }

  def apply(vs: Int*): conj = {
    var x = new conj(new TreeSet);

    for (v <- vs) {
      x = x * v.toShort
    }
    x
  }
}
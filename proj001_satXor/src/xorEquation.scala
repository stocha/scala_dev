

import scala.collection.immutable.Vector
import scala.collection.immutable.TreeSet


class conj extends Ordered[conj]{
  val it=TreeSet[Short]();
  
  def compare( b:conj) : Int = {
    -1
    }
  
}

class xor  extends Ordered[xor]{
  val it=TreeSet[conj]();
  
  def compare( b:xor) : Int = {
    -1
    }  
}

class equation{
   val it=TreeSet[xor]() ;
}

object T {
  
   def apply : conj ={
    new conj;
  }
   
   def apply(v : Short*) : conj ={
    new conj;
  }   
  
}
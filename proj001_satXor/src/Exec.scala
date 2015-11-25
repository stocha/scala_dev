

/**
 * @author Jahan
 */

import T._

object Tests {

  val dec = 10

  def t(a: Int, b: Int) = {
    T(a, (b + dec))
  }

  def t(b: Int) = {
    T(b + dec)
  }

  def doT1 {

    println("coucou doT1");

    val x = t(0, 0)
    val y = t(0, 1) + t(1, 0)
    val z = t(0, 2) + t(1, 1) + t(2, 0)
    val a = t(0, 3) + t(1, 2) + t(2, 1) + t(3, 0)

    /*  println(" " + x);
  println(" " + y);
  println(" " + z);
  println(" " + a);

  println("" + (x ** y ** z))*/

    val ox = T(2) + t(2)
    val oy = T(1) + t(1)
    val oz = t(0, 0)
    val oa = (T(1) ** (T() + T(2))) + (T(2) ** (T() + T(1)))
    /*
  println(" " + ox);
  println(" " + oy);
  println(" " + oz);
  println(" " + oa);

  println("" + (ox ** oy))

  println("" + T.diag(0, 12));
  println("" + T.diag(1, 12));
  println("" + T.diag(2, 12));
  println("" + T.diag(3, 12));
  println("" + T.diag(4, 12));
  println("" + T.diag(5, 12));*/

    val sz = 16;

    var l: xor = T()
    for (i <- 0 until sz) {
      // l= l ** T.diag(i, sz)

      println("" + T.diag(i, sz))
      //println("    "+l)
    }

    println(".. " + T.diag(4, 12) ** T.diag(5, 12));

    val p0 = T(1) * t(2) + T(2) * t(1)
    val p2 = T(2) + t(2)
    val p1 = T(1) + t(1)

    val p0conc = T(1) + T(2)
    val p0conc2 = t(1) + t(2)

    l = T()

    l = l ** p0
    l = l ** p1
    l = l ** p2
    println(" " + p0)
    println(" " + p1)
    println(" " + p2)
    println(" " + l)

    l = T()
    l = l ** p1
    l = l ** p2
    l = l ** p0conc
    l = l ** p0conc2
    println(" " + l)
  }

}

object Exec extends App {
  Tests.doT1
}
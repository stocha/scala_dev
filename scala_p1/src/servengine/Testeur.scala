package servengine

/**
 * @author Jahan
 */
object Testeur extends App {

  def testBm_basics {

    println("Test, Bm!")
    val b = BitMap.zero

    println("" + b);

    println("" + BitMap.full);

    println("" + (BitMap.full--));

    println("" + (BitMap.full++));

    println("" + ((((BitMap.full<<)>>)>>)));

    println("" + ((((BitMap.full>>)<<)<<)));

    {
      
      println("Test set");
      var x = (BitMap.zero)
      x = x.set(0)(0)(1)
       println("x.set(0)(0)(1)\n" +x);
      x = x.set(10)(10)(1)
       println("x.set(10)(10)(1)\n" +x);
      x = x.set(11)(11)(1)
       println("x.set(11)(11)(1)\n" +x);
      x = x.set(12)(11)(1)
      println("x.set(12)(11)(1)\n" +x);

      x= x.set(11)(11)(0)
      println("" +x);  
    }

  }

  def testServ {
    println("Test, world!")
    val serv = new ServWorld(1154, false, false, List(new randBot(), new randBot()))

    for (i <- 0 until 100) {
      serv.applyTurn()

    }
  }

  override def main(args: Array[String]) {
    testBm_basics
  }
}
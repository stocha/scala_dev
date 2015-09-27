package other
import scala.util.Random


object ServWorld {
  val W = 35
  val H = 20
  val DUR2 = 350
  val DUR3 = 300
  val DUR4 = 250
  val MAXBACK = 25

}
  

class servMap {
  val dat = Array.fill[Int](35,20) { -1 }

}

class servCoord(
    val x: Int, val y: Int, val back: Int) {

  override def toString = {
    " " + x + " " + y
  }

}

abstract class servBot {
  def init(nbPlay: Int, idP: Int)
  def input(coords: Array[servCoord], dat: servMap)
  def turn: servCoord
  def name: String
}

object Player extends App {
  val W: Int = 35
  val H: Int = 20

  def convertMap(sl : Seq[String]) = {
    val res = new servMap
    

    var j=0;
    for(s <- sl){
      var i=0;
      for(c <- s){
        c match{
          case '.' => res.dat(i)(j)= -1;
          case '0' => res.dat(i)(j)= 0;
          case '1' => res.dat(i)(j)= 1;
          case '2' => res.dat(i)(j)= 2;
          case '3' => res.dat(i)(j)= 3;
          
        }
        
        i=i+1
      }
      
      j=j+1;
    }
    res
  }
  
  val myBot: servBot = new Bot001();
  val opponentcount = readInt // Opponent count

  myBot.init(opponentcount, 0);
  // game loop
  while (true) {
    val gameround = readInt
    // x: Your x position
    // y: Your y position
    // backintimeleft: Remaining back in time
    val someCoord = for (i <- 0 until (opponentcount + 1)) yield {
      // opponentx: X position of the opponent
      // opponenty: Y position of the opponent
      // opponentbackintimeleft: Remaining back in time of the opponent
      val Array(opponentx, opponenty, opponentbackintimeleft) = for (i <- readLine split " ") yield i.toInt
      new servCoord(opponentx, opponenty, opponentbackintimeleft)

    }

    val ls = for (i <- 0 until 20) yield {
      val line = readLine // One line of the map ('.' = free, '0' = you, otherwise the id of the opponent)
      line
    }
    
    val map=convertMap(ls)
    myBot.input(someCoord.toArray, map)
    
    val order=myBot.turn

    // Write an action using println
    // To debug: Console.err.println("Debug messages...")

    System.err.println("Round " + gameround);
    println("" + order.x + " " + order.y) // action: "x y" to move or "BACK rounds" to go back in time
  }
}


//----------------------------------------------------------------



class Bot001 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  override def init(nbPlay: Int, idP: Int) {
    this.nbPlay = nbPlay;
    this.idP = idP;

    val r = new Random(0xFF88773);
    var ssN = 0x77889E76;
    for (i <- 0 until 10 * (idP + 10)) {
      ssN = (ssN << idP) ^ r.nextInt()
    }
    rand = new Random(ssN)
  }
  override def input(coords: Array[servCoord], dat: servMap) {
    coord = coords(idP)
  }

  override def turn: servCoord = {

    val W = ServWorld.W;
    val H = ServWorld.H;

    def clipx(x: Int) = x match {
      case a if (a < 0)  => 0
      case a if (a >= W) => W - 1
      case _             => x
    }

    def clipy(x: Int) = x match {
      case a if (a < 0)  => 0
      case a if (a >= H) => H - 1
      case _             => x
    }

    val x = rand.nextInt(3) + coord.x - 1;
    val y = rand.nextInt(3) + coord.y - 1;

    val flip = rand.nextBoolean()

    if (flip)
      new servCoord(coord.x, clipy(y), 0);
    else
      new servCoord(clipx(x), coord.y, 0);
  }
  override def name: String = {
    "Default (" + idP + ")";
  }
  
}

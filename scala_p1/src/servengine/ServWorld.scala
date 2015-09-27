

package servengine
import scala.util.Random

/**
 * @author Jahan
 */

object ServWorld {
  val W = 35
  val H = 20
  val DUR2 = 350
  val DUR3 = 300
  val DUR4 = 250
  val MAXBACK = 25

}

class servMap {
  val dat = Array.fill[Int](ServWorld.W, ServWorld.H) { -1 }

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

class randBot extends servBot {
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

class ServWorld(seed: Int, sameSquare: Boolean, symetrical: Boolean, bots: List[servBot]) {
  val mapTraj = new Array[servMap](bots.size)
  val mapBonus = new Array[servMap](bots.size)
  val officialMap = new servMap()
  val owner = Array.fill(bots.size)(BitMap.zero)
  var rawTurn = List[List[servCoord]]();

  var turn = 0

  val rand = new Random(seed)

  { // Init world

    val uniqueCoord = new servCoord(rand.nextInt(ServWorld.W), rand.nextInt(ServWorld.H), 1)
    val lco = for (b <- bots) yield {
      val scoord = if (sameSquare) uniqueCoord else { new servCoord(rand.nextInt(ServWorld.W), rand.nextInt(ServWorld.H), 1) }
      scoord
    }

    rawTurn = lco :: rawTurn
    applyRawTurn(lco)

    var id = 0;

    for (b <- bots) {
      b.init(bots.size, id)
      id = id + 1;
    }

  }

  def applyRawTurn(raw: List[servCoord]) {

    val pointed = new scala.collection.mutable.HashSet[Tuple2[Int, Int]]();
    val canceled = new scala.collection.mutable.HashSet[Tuple2[Int, Int]]();

    var id = 0;
    for (tr <- raw) {
      val at = new Tuple2(tr.x, tr.y);

      val p = pointed.contains(at);
      if (officialMap.dat(tr.x)(tr.y) == -1 && !p) {
        pointed.add(at)
      } else {
        canceled.add(at)
      }

      id = id + 1;
    }

    id = 0;
    for (tr <- raw) {
      val at = new Tuple2(tr.x, tr.y);
      val c = canceled.contains(at)
      if (!c) {
        officialMap.dat(tr.x)(tr.y) = id;
        owner(id)=owner(id).set(tr.x)(tr.y)(1)
        
        val capture=BitMap.enclosed(owner, id);
        if(!capture.isNull){
          capture.forAllSet{
            (x : Int, y : Int) => { officialMap.dat(x)(y) = id; }
          }
        }
      }

      id = id + 1;
    }
  }

  def applyTurn() {
    turn = turn + 1;

    var id = 0;
    val tM = for (b <- bots) yield {
      b.input(rawTurn.head.toArray, officialMap)

      val tr = b.turn
      id = id + 1
      tr
    };
    rawTurn = tM :: rawTurn;

    applyRawTurn(tM)

    System.err.println("turn : " + turn);
    System.err.println(" " + tM);
    for (b <- bots) {
      System.err.print(b.name + " ")
    }    
    System.err.println;
    
    for (id <- 0 until bots.size) {
      System.err.print(" | "+ bots(id).name + " score -> "+ owner(id).countBitset )
    }    
    System.err.println;    
    
    //for (b <- owner) {
    //  System.err.print(""+b+"\n")
    //}  
    
   // for( i <- 0 until bots.size){
   //   System.err.print(""+BitMap.enclosed(owner, i)+"\n")
   // }
    
    System.err.println;    

  }

}
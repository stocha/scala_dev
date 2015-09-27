package servengine

import scala.util.Random

/**
 * @author Jahan
 */
class Bot003 extends servBot {
  var nbPlay = -1;
  var idP = -1;
  var coord = new servCoord(0, 0, 0)
  var rand: Random = null

  var coords: Array[servCoord] = null
  var dat: servMap = null

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

    this.coords = coords
    this.dat = dat
  }

  override def turn: servCoord = {

    val W = ServWorld.W;
    val H = ServWorld.H;

    val bms = dat.extractBm(coords.size);
    val firstZone = BitMap.firstArea(bms, coords, idP)
    
    val void = BitMap.voidArea(bms)
    
    val consolidatedZone = (firstZone | bms(idP))
    val bordFirstExt= ( consolidatedZone).scramble ^ consolidatedZone 
    val bordFirst = (bordFirstExt.scramble & consolidatedZone ) & (~bms(idP))

    if (!(bordFirst.isNull)) {
      val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), bordFirst)

      //println("\n"+BitMap.zero.set(coord.x)(coord.y)(1));
      //println("\n"+firstZone);
      //println("\n"+(firstZone | bms(idP)));
      
      //println("bordFirst\n"+bordFirst);
      //println(""+possibi);

      val rx = rand.nextInt(possibi.size)
      val dir = possibi(rx)
      dir match {
        case 0 => new servCoord(coord.x, coord.y - 1, 0);
        case 1 => new servCoord(coord.x + 1, coord.y, 0);
        case 2 => new servCoord(coord.x, coord.y + 1, 0);
        case 3 => new servCoord(coord.x - 1, coord.y, 0);

      }

    } else {
      val v = BitMap.voidArea(bms)

      if (v.isNull) {
        new servCoord(coord.x, coord.y, 0);
      } else {
        val possibi = BitMap.firstDirTo(BitMap.zero.set(coord.x)(coord.y)(1), v)
        val rx = rand.nextInt(possibi.size)
        val dir = possibi(rx)
        dir match {
          case 0 => new servCoord(coord.x, coord.y - 1, 0);
          case 1 => new servCoord(coord.x + 1, coord.y, 0);
          case 2 => new servCoord(coord.x, coord.y + 1, 0);
          case 3 => new servCoord(coord.x - 1, coord.y, 0);

        }
      }

    }

  }
  override def name: String = {
    "Bot003 (" + idP + ")";
  }

}
package servengine

import scala.util.Random

/**
 * @author Jahan
 */
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
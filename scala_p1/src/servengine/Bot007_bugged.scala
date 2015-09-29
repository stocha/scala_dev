package servengine

import scala.util.Random

/**
 * @author Jahan
 */
class Bot007Defender extends servBot {
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
  
  def goTarget(pos : servCoord,targs : List[BitMap], void : BitMap) : servCoord ={
    
    if(targs.isEmpty){
      pos
    }else
    if(!targs.head.isNull){
        val currMap=BitMap.zero.set(pos.x)(pos.y)(1)
         //Console.err.println("currMap\n"+currMap);
        //Console.err.println("dest\n"+targs.head);
        //Console.err.println("void\n"+void);
          val possibi = BitMap.firstDirToThrough(currMap, targs.head,void)
         // Console.err.println("possibi Through void "+possibi);
          if(possibi.nonEmpty){
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)
          }else{
              val possibi = BitMap.firstDirTo(BitMap.zero.set(pos.x)(pos.y)(1), targs.head)
                  val rx = rand.nextInt(possibi.size)
                  val dir = possibi(rx)
                  pos.dirToCoord(dir)            
          }
          
    }else{
      goTarget(pos,targs.tail,void)
    }

  }
  
  def stratBase ={
    
        val maxFirstLim = coords.size match {
          case 2 => 9
          case 3 => 9
          case 4 => 9
          
        }        
        
        val trig = coords.size match {
          case 2 => 25
          case 3 => 14
          case 4 => 14
          
        }        
            
    
      def maxOrEmpty(y : List[BitMap])={
        if(y.isEmpty) BitMap.zero else
        {
          y.maxBy { x => x.countBitset }
        }
      }
      
      def extractMax(x : BitMap,minCount : Int)={
        val map=maxOrEmpty(x.extractZones)
        if(map.countBitset < minCount) BitMap.zero else map
      }
    
    
        val bms = dat.extractBm(coords.size);
        val terri= bms(idP)
        val void = BitMap.voidArea(bms)
        val firstZone = BitMap.rawFirstMap( coords, idP)
        
        val playerPosMap=coords.map( x=> BitMap.zero.set(x.x)(x.y)(1))
        val me = playerPosMap(idP)
        val ePoint=playerPosMap.foldLeft(BitMap.zero){_ | _}^me
        //Console.err.println("firstZone\n"+firstZone);
        //Console.err.println("firstZone noyau\n"+firstZone.noyau);
        
        val maxFirstZone= extractMax(firstZone&(void |terri),maxFirstLim)
        //Console.err.println("maxFirstZone\n"+maxFirstZone);
        
        
         //Console.err.println("ePoint\n"+ePoint);
        val (dist,clothestTo)=(maxFirstZone & void).closestPointHere(ePoint)
        //Console.err.println("clothestTo\n"+clothestTo);
        
        //val noyau= (firstZone );

        val maxEmpty = extractMax(void,0)
      //  Console.err.println("maxEmpty\n"+maxEmpty);
        
        
        //val cross=BitMap.zero.paintCrossAt(coord.x, coord.y) & void // important : uniquement les cases vides vieux !!
       // val captIfCross=extractMax(BitMap.enclosed((terri | cross), void  & (~cross)) & firstZone , trig)
       // Console.err.println("captIfCross\n"+captIfCross);
                     
        
        val bordFirstZone = (( maxFirstZone | terri).frontierMap ) &  (~terri)      
        //Console.err.println("bordFirst\n"+bordFirstZone);
        
        goTarget(coord, List(clothestTo,bordFirstZone,void), void)
        
  }

  override def turn: servCoord = {
    stratBase

  }
  override def name: String = {
    "Bot007Defender (" + idP + ")";
  }

}
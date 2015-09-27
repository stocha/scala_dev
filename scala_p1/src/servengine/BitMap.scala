package servengine

/**
 * @author Jahan
 */

object BitMap{
  
  val zero = {
    new BitMap(
        0,
        0,
        0,
        0,
        0,
        
        0,
        0,
        0,
        0,
        0,
        
        0,
        0,
        0,
        0,
        0,
        
        0,
        0,
        0,
        0,
        0
                
        )
    
  }
  
  val umask={
    (-1L)>>>(64-35);
  }
  
  val full = {
    new BitMap(
        umask,
        umask,
        umask,
        umask,
        umask,
        
        umask,
        umask,
        umask,
        umask,
        umask,
        
        umask,
        umask,
        umask,
        umask,
        umask,
        
        umask,
        umask,
        umask,
        umask,
        umask
                
        )    
  }
  
}

class BitMap(
    val u00 :Long,
    val u01 :Long,
    val u02 :Long,
    val u03 :Long,
    val u04 :Long,
    
    val u05 :Long,
    val u06 :Long,
    val u07 :Long,
    val u08 :Long,
    val u09 :Long,    
    
    val u10 :Long,
    val u11 :Long,
    val u12 :Long,
    val u13 :Long,
    val u14 :Long,
    
    val u15 :Long,
    val u16 :Long,
    val u17 :Long,
    val u18 :Long,
    val u19 :Long //,       
    
    //val end : Long

){
  
  def l_getAt(at : Int) : Long = {
    at match{
      case 0 => u00
      case 1 => u01
      case 2 => u02
      case 3 => u03
      case 4 => u04
      
      case 5 => u05
      case 6 => u06
      case 7 => u07
      case 8 => u08
      case 9 => u09      
      
      case 10 => u10
      case 11 => u11
      case 12 => u12
      case 13 => u13
      case 14 => u14
      
      case 15 => u15
      case 16 => u16
      case 17 => u17
      case 18 => u18
      case 19 => u19                
    }    
  }
  
  def l_setAt(at : Int) (vvv : Long) : BitMap = {
    
    //new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
    at match{
      case 0 => new BitMap(vvv,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 1 => new BitMap(u00,vvv,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 2 => new BitMap(u00,u01,vvv,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 3 => new BitMap(u00,u01,u02,vvv,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 4 => new BitMap(u00,u01,u02,u03,vvv, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      
      case 5 => new BitMap(u00,u01,u02,u03,u04, vvv,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 6 => new BitMap(u00,u01,u02,u03,u04, u05,vvv,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 7 => new BitMap(u00,u01,u02,u03,u04, u05,u06,vvv,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 8 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,vvv,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 9 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,vvv, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19)      
      
      case 10 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, vvv,u11,u12,u13,u14, u15,u16,u17,u18,u19)
      case 11 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,vvv,u12,u13,u14, u15,u16,u17,u18,u19)
      case 12 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,vvv,u13,u14, u15,u16,u17,u18,u19)
      case 13 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,vvv,u14, u15,u16,u17,u18,u19)
      case 14 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,vvv, u15,u16,u17,u18,u19)
      
      case 15 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, vvv,u16,u17,u18,u19)
      case 16 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,vvv,u17,u18,u19)
      case 17 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,vvv,u18,u19)
      case 18 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,vvv,u19)
      case 19 => new BitMap(u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,vvv)                
    }    
  }  
  
  def l_toString(at : Int) = {
      val r= l_getAt(at)
      llong_toString(r)
  }
  
  def llong_toString(r : Long) = {
    var res="";
    
    for(i <- 0 until 35){
      val c = if(( (r>>>i)&1L) ==0) "- " else "# ";
      res+=c;
    }
    res
  }  
  
  def get(x :Int)(y : Int) ={
    val r=l_getAt(y)
    (r>>>x)&1L
  }
  
  def set(x :Int)(y : Int)(v : Long) = {
    val r : Long=l_getAt(y)
    val b : Long=(1L<<x)
    val nb : Long = (~b & r) | ((v<<x)&b)
    //println("old "+llong_toString(r));
    //println("new "+llong_toString(nb));
    val res=l_setAt(y) (nb)
    //println("res is \n"+res);
    res
  }  
  
  override def toString() ={
    var res="";
    
    for(i <- 0 until 20){
      res+=l_toString(i);
      res+="\n";
    }    
    res
  }
  
  def ++ ={
    new BitMap(0L,u00,u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18)

  }
  
  def -- ={
    new BitMap(u01,u02,u03,u04, u05,u06,u07,u08,u09, u10,u11,u12,u13,u14, u15,u16,u17,u18,u19,0L)
  }  
  
  def << ={
    //System.err.println("apply << ");
    new BitMap(u00>>>1,u01>>>1,u02>>>1,u03>>>1,u04>>>1, u05>>>1,u06>>>1,u07>>>1,u08>>>1,u09>>>1, u10>>>1,u11>>>1,u12>>>1,u13>>>1,u14>>>1, u15>>>1,u16>>>1,u17>>>1,u18>>>1,u19>>>1)
  }    
  
  def >> ={
    new BitMap(BitMap.umask & u00<<1,BitMap.umask & u01<<1,BitMap.umask & u02<<1,BitMap.umask & u03<<1,BitMap.umask & u04<<1, BitMap.umask & u05<<1,BitMap.umask & u06<<1,BitMap.umask & u07<<1,BitMap.umask & u08<<1,BitMap.umask & u09<<1, BitMap.umask & u10<<1,BitMap.umask & u11<<1,BitMap.umask & u12<<1,BitMap.umask & u13<<1,BitMap.umask & u14<<1, BitMap.umask & u15<<1,BitMap.umask & u16<<1,BitMap.umask & u17<<1,BitMap.umask & u18<<1,BitMap.umask & u19<<1)
  }     
  
  def <<- ={
        new BitMap(u01>>>1,u02>>>1,u03>>>1,u04>>>1, u05>>>1,u06>>>1,u07>>>1,u08>>>1,u09>>>1, u10>>>1,u11>>>1,u12>>>1,u13>>>1,u14>>>1, u15>>>1,u16>>>1,u17>>>1,u18>>>1,u19>>>1,0L)

  }
  
  def <<+ ={
    new BitMap(0L,u00>>>1,u01>>>1,u02>>>1,u03>>>1,u04>>>1, u05>>>1,u06>>>1,u07>>>1,u08>>>1,u09>>>1, u10>>>1,u11>>>1,u12>>>1,u13>>>1,u14>>>1, u15>>>1,u16>>>1,u17>>>1,u18>>>1)

  }  
  
  def >>- ={
    new BitMap(BitMap.umask & u01<<1,BitMap.umask & u02<<1,BitMap.umask & u03<<1,BitMap.umask & u04<<1, BitMap.umask & u05<<1,BitMap.umask & u06<<1,BitMap.umask & u07<<1,BitMap.umask & u08<<1,BitMap.umask & u09<<1, BitMap.umask & u10<<1,BitMap.umask & u11<<1,BitMap.umask & u12<<1,BitMap.umask & u13<<1,BitMap.umask & u14<<1, BitMap.umask & u15<<1,BitMap.umask & u16<<1,BitMap.umask & u17<<1,BitMap.umask & u18<<1,BitMap.umask & u19<<1,0L)
  }
  
  def >>+ ={
    new BitMap(0L,BitMap.umask & u00<<1,BitMap.umask & u01<<1,BitMap.umask & u02<<1,BitMap.umask & u03<<1,BitMap.umask & u04<<1, BitMap.umask & u05<<1,BitMap.umask & u06<<1,BitMap.umask & u07<<1,BitMap.umask & u08<<1,BitMap.umask & u09<<1, BitMap.umask & u10<<1,BitMap.umask & u11<<1,BitMap.umask & u12<<1,BitMap.umask & u13<<1,BitMap.umask & u14<<1, BitMap.umask & u15<<1,BitMap.umask & u16<<1,BitMap.umask & u17<<1,BitMap.umask & u18<<1)
  }  
  
}


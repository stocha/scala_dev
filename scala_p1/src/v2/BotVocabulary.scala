package v2

/**
 * @author Jahan
 */
class BotVocabulary(val  st : GameState4P) {
  val void = st.tr.void
  val me = st.pos.pos0
  
  
  def currZones = {
    val near= me.scramble & void
    val trail=BMap.followTrail(near, void)
    trail.split
    
  }
  
  
  def firstZoneHeuristic = {
    var e = st.pos.pos1 | st.pos.pos2 | st.pos.pos3;
    var f = st.pos.pos0;

    var firste = BMap.zero;
    
    if(!e.isNull && !f.isNull){
      while (!(~(f | e)).isNull) {
        e = e.scramble
        f = f.scramble
        firste = firste | (~e & f)
      }
  
      (firste )      
    }else{
      BMap.full
    }
        

  }    
  
  def goTo(to : BMap) = {
    BMap.firstDirTo(me, to)
  }
  
  
  def nthBm(to : BMap, nb : Int)(code : BMap => BMap) : BMap={
    if(nb ==0) to else{
      nthBm( code(to) , nb-1)(code)
    }
  }
  
  
}
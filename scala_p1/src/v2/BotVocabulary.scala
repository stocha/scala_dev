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
  
  def simpleSquareRuleZone = {
    val ref= st
    val bv = new BotVocabulary(ref)
    val first=bv.firstZoneHeuristic
    val void =ref.tr.void
    
    val allFirstEmpty=(first&void).split
    val maxfirst= if(allFirstEmpty.size>0) {allFirstEmpty.maxBy { x => x.countBitset } }
     else{
       if(void.isNull){
         void
       }else{
         void.split.maxBy { x => x.countBitset }
       }
     }
    
    val area=(bv.nthBm(maxfirst.noyau, 3){x => x.angularScramble })&(void|ref.tr.pos0)        
    (area & (first&void))
  }
  
  def goTo(to : BMap) = {
    BMap.firstDirTo(me, to)
  }
  
  
  def nthBm(to : BMap, nb : Int)(code : BMap => BMap) : BMap={
    if(nb ==0) to else{
      nthBm( code(to) , nb-1)(code)
    }
  }
  
  def forsee_withZerger(to : BMap, plan : agentAbstract) ={
    val zerg=new bv_followTrail(to)(identity)
    val sim =  new SimulBot(0,st,Array(plan,zerg,zerg,zerg))
        var dir= 0
        
        while(GameState4P.m(dir)(0)!=4){
          dir = sim.turn()
        }
        
        sim.getState
  }
  
  def forsee_withSquarers(to : BMap, plan : agentAbstract) ={
    val zerg=new tb001
    val sim =  new SimulBot(0,st,Array(plan,zerg,zerg,zerg))
        var dir= 0
        
        while(GameState4P.m(dir)(0)!=4){
          dir = sim.turn()
        }
        
        sim.getState
  }  
}
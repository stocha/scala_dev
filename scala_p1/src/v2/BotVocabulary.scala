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
  
  def goTo(to : BMap) = {
    BMap.firstDirTo(me, to)
  }
  
}
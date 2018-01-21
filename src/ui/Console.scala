package ui

import planets._
import planets.constants._
import scala.math._

/**Contains the most important 
 * information about the simulation*/
class Console(planets: Array[SpaceObject])(implicit p: PlanetWindow) {
  
  /*Take just the satellites 
   * from the planets collection*/
  private val satellites = planets.collect( { case s: Satellite => s } )
  private val earth      = planets.find(_.isInstanceOf[Earth]).getOrElse(null)
  private val sun        = planets.find(_.isInstanceOf[Sun]).getOrElse(null)
  private var index      = 0
  private val size       = planets.size
  
  /**Select the satellite that is
   * first in line*/
  if(tracking.isDefined) {tracking.get.select()}
  
  /**Change the satellite
   * in observation*/
  def move(by: Int) = {
    if(!satellites.isEmpty) {
      satellites(index).untrack()
      satellites(index).deSelect()
      index += by
      if(index < 0)
        index = satellites.size + index
      else
        index %= satellites.size
      satellites(index).select()
    }
  }
  
  /**Returns the currently tracked satellite 
   * in an option -wrapper if there are satellites
   * but if not returns None*/
  def tracking = if(satellites.isEmpty) None else Some(satellites(index))
  
  /**Displays important
   * information about the simulation*/
  def display() = {
    /*Trims a double value into a more
     * presentable form*/
    implicit def trimDouble(d: Double): String = {
      var distance = d
      val pwr = max(log10(distance), 0).round
      val p = pwr.toInt
      if(p != 0 && p != 1) {distance /= pow(10, p - 1)}
      val s = distance.toString
      var res = s.takeWhile(_ != '.') + s.dropWhile(_ != '.').take(3)
      if(p != 0 && p != 1) {res += " E " + (p - 1)}
      return res
    }
    val consoleWidth  = 300
    val consoleHeight = 300
    val consoleX      = p.width - consoleWidth - 50
    val consoleY      = 50
    var fontSize      = 30
    p.textSize(fontSize/2)
    p.fill(150, 100)
    p.noStroke()
    p.rect(consoleX, consoleY, consoleWidth, consoleHeight)
    /*Color of 
     * the line*/
    p.fill(255)
    if(!satellites.isEmpty) {
      p.textSize(fontSize)
      p.text(satellites(index).name, consoleX + 5, consoleY + fontSize)
      p.textSize(fontSize / 2)
      val size = satellites.size
      /*There is no need for using metersperpixel since we can assume
       * the update rate is metersperpixel times larger than it actually is*/
      var v: String = satellites(index).getVelocity.magnitude / 1e3
      val mass: String = satellites(index).mass
      p.text(s"Velocity: $v km/s", consoleX + 10, consoleY + 70)
      p.text(s"Satellites currently in orbit: $size", consoleX + 10, consoleY + 90)
      p.text(s"Satellites mass: $mass", consoleX + 10, consoleY + 110)
      if(earth != null) {
        var distance: String = satellites(index).getLocation.sub(earth.getLocation).magnitude / 1e3
        p.text(s"Distance from earth: $distance km", consoleX + 10, consoleY + 130)
      }
    } else {
        p.textSize(fontSize)
        p.text("No satellites", consoleX + 5, consoleY + 35)
    }
      p.textSize(fontSize / 2)
      p.strokeWeight(1f)
      p.stroke(255)
      //line separating satellite info from system info
      p.line(consoleX, consoleY + consoleHeight/2,
          consoleX+consoleWidth, consoleY+consoleHeight/2)
      p.text(s"Amount of objects in the system: $size", consoleX + 10, consoleY + 185)
      val planet = p.getTarget().name
      p.text(s"Camera target: $planet", consoleX + 10, consoleY + 205)
      val vel: String = p.getTarget().getVelocity.magnitude / 1e3
      p.text(s"$planet velocity: $vel km/s", consoleX + 10, consoleY + 225)
      val m: String = p.getTarget().mass
      p.text(s"$planet mass: $m kg", consoleX + 10, consoleY + 245)
      if(sun != null && p.getTarget() != sun) {
        val distance: String = p.getTarget().getLocation.sub(sun.getLocation).magnitude / 1e3
        p.text(s"Distance from sun: $distance km", consoleX + 10, consoleY + 265)
      }
  }
  
  
}
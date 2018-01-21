package ui

import planets._
import javax.swing.JFrame
import javax.imageio.ImageIO
import java.io.File
import java.awt.Image
import processing.core.PApplet


/**Object for starting 
 * the simulation*/
object Start  {

  def main(args: Array[String]) = {
    PApplet.main("planets.PlanetWindow")
  }
  
}
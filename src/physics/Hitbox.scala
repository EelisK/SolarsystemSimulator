package physics

import planets._
import planets.constants._
import java.awt.geom._
import java.awt.Rectangle
import processing.core.PApplet

/**Spherical hitbox for detecting collisions*/
class Hitbox(width: Int, height: Int, depth: Int, o: SpaceObject) {
  
  def getHeight(): Double = height
  def getWidth(): Double = width
  def getX(): Double = o.getLocation.i
  def getY(): Double = o.getLocation.j
  def getZ(): Double = o.getLocation.k
  def getRadius = o.radius
  
  /*Returns true if objects intersect,
   * false otherwise*/
  def intersects(h: Hitbox): Boolean = {
    val vec = new Vector3D((getX - h.getX), (getY - h.getY), (getZ - h.getZ()))
    val intersection = vec.magnitude <= (h.getRadius + getRadius) * metersPerPixel
    return intersection
  }
  
  /**A function for displaying
   * the spherical hitbox. Good 
   * for debugging purposes*/
  def draw[T <: PApplet](t: T) = {
    t.pushMatrix()
    t.translate(getX().toFloat, getY().toFloat, getZ().toFloat)
    t.sphere(getRadius.toFloat)
    t.popMatrix()
  }
  
}
package planets

import physics._
import planets.constants._

/**Describes the spaceobjects 
 * current position and velocity*/
final class State(var pos: Vector3D = Vector3D(), var vel: Vector3D = Vector3D()) {
  def add(d: Derivate) = {
    pos = pos.add(d.velocity)
    vel = vel.add(d.acceleration)
  }
  def toDerivate() = {
    Derivate(pos, vel)
  }
  
}
/**Companion object of class State*/
object State {
  def apply() = new State()
  def apply(p: Vector3D, v: Vector3D) = new State(p, v)
}
/**Derivate object for simplifying the
 * calculations of effects
 * of other spaceobject*/
final class Derivate(var velocity: Vector3D = Vector3D(), var acceleration: Vector3D = Vector3D()) {
  def toState() = {
    State(velocity, acceleration)
  }
}
/**Companion object of class Derivate*/
object Derivate {
  def apply() = new Derivate()
  def apply(v: Vector3D, a: Vector3D) = new Derivate(v, a)
}
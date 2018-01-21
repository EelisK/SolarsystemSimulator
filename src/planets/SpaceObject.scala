package planets

import physics._
import planets.constants._
import processing.core._
import scala.math._

/**An abstract class that works as a
 * basis for all heavenly bodies*/
abstract class SpaceObject(location: Vector3D, val mass: Double, 
                           velocity: Vector3D, t: PApplet) {
  
  self: SpaceObject =>
  
  /*By default the velocity vector is 0*/
  def this(location: Vector3D, mass: Double, t: PApplet) = {
    this(location, mass, Vector3D(0, 0, 0), t)
  }
  
  /*These values need to be 
   * specified in the class that 
   * extends to this one*/
  val radius: Double;
  val name: String;
  val fileName: String;
  val icon: PImage;
  val globe: PShape;
  val angularVelocity: Double;
  
  /*Objects current angle.
   * Should be rotated this much
   * when displayed*/
  private var angle = 0d
  /*Describes the objects location and velocity*/
  private var state = State(location, velocity)
  /*This vector is only for displaying the
   * normal acceleration*/
  private var aV = Vector3D()
  def getLocation = state.pos
  def getVelocity = state.vel
  def getAngle    = angle
  /**Returns the spaceobjects
   * location on the screen, not the
   * actual location. Method
   * <strong>getLocation</strong> 
   * returns the actual location*/
  def getScaledLocation = state.pos.div(metersPerPixel)
  
  /**Avoiding multiple .toFloat calls
   * on double values since alot of
   * parameters in various methods
   * need to be float values*/
  implicit def toFloat(d: Double) = d.toFloat
  
  /*Helps simplifying 
   * some expressions*/
  implicit class double(n: Double) {
    def *(v: Vector3D): Vector3D = {
      v.multiply(n)
    }
  }
  
  /**Displays the spaceobject on the screen*/
  def display(tangent: Boolean) = {
    t.pushMatrix()
    val loc = getScaledLocation
    /*Translation from the objects location
     * so that the sphere is drawn in the right place*/
    t.translate(loc.i, loc.j, loc.k)
    t.fill(0, 0, 0, 255)
    t.fill(255)
    if(tangent) {tangentLine();normalAcceleration()}
    t.rotateZ(angle.toDegrees)
    /*Rotate the object -90 degrees
     * so it looks better*/
    t.rotateX(-Pi / 2)
    t.shape(globe)
    t.popMatrix()
  }
  
  /**The Runge-Kutta algorithm*/
  def update(ps: Array[SpaceObject], dt: Double) = {
    angle = (angle + angularVelocity.toRadians * dt) % (2 * Pi)
    /*Method for getting the combined
     * acceleration caused by each
     * spaceobject in the simulation*/
    def acceleration(): Vector3D = {
      var accelerationVector = Vector3D()
      for ( planet <- ps ) {
        if(planet != this) {
          var direction = Vector3D.sub(planet.getLocation,getLocation)
          /*Distance in meters*/
          val r = direction.magnitude
          val normal = direction.normalize
          /*
           * F = y*(m1*m2)/(r^2)
           * F = m * a
           * ==>>
           * a = y*m2/(r^2)
           * */
          val forceMagnitude = ((gravitationalConstant * planet.mass) / r) / r
          accelerationVector = 
            accelerationVector.add( normal multiply forceMagnitude )
        }
      }
      /*The displayed vector*/
      aV = accelerationVector
      accelerationVector
    }
    
    /**returns the derivates of location and velocity,
     * meaning velocity and acceleration */
    def evaluate(initState: State, derivate: Derivate, dt: Double): Derivate = {
      
      val state = State()
      state.pos = initState.pos.add(derivate.velocity * dt)
      state.vel = initState.vel.add(derivate.acceleration * dt)
      
      val output  = Derivate()
      output.velocity = state.vel
      output.acceleration = acceleration()
      return output
    }
    /**Evaluate the expressions*/
    val result1 = evaluate(state, Derivate(), 0d)
    val result2 = evaluate(state, result1, 0.5 * dt)
    val result3 = evaluate(state, result2, 0.5 * dt)
    val result4 = evaluate(state, result3, dt)
    /*And calculate their weighter average*/
    val dxdt = 1d / 6d * (result1.velocity + 2d * (result2.velocity + result3.velocity) + result4.velocity)
    val dvdt = 1d / 6d * (result1.acceleration + 2d * (result2.acceleration + result3.acceleration) + result4.acceleration)
    state.add(Derivate(dxdt multiply dt, dvdt multiply dt))
    /*Limit the velocity vector to be at
     * max light speed (unrealistic situation
     * but necessary in case user tries to create
     * planets with infinitely large velocity*/
    state.vel = state.vel.limit(lightSpeed)
  }
  
  def getX = state.pos.i
  def getY = state.pos.j
  def getZ = state.pos.k
  
  def intersects(another: SpaceObject): Boolean = {
    getHitbox.intersects(another.getHitbox)
  }
  
  /**This method should be called
   * if a collision is detected*/
  def dealWithCollision(so: SpaceObject): Unit = {
    /*Minimum distance the objects must have between them*/
    val minimumDistance = {
      val v = getScaledLocation.sub(so.getScaledLocation)
      v.multiply((radius + so.radius - v.magnitude) / v.magnitude)
    }
    /*Separate the objects based on their mass*/
    state.pos = state.pos.add(minimumDistance.multiply(so.mass / (mass + so.mass) * 1.1))
    so.state.pos = so.state.pos.sub(minimumDistance.multiply(mass / (mass + so.mass) * 1.1))
    state.vel = 
      (state.vel * (mass - so.mass) + so.state.vel * 2 * so.mass).div(mass + so.mass)
    so.state.vel = 
      (so.state.vel * (so.mass - mass) + state.vel * 2 * mass).div(mass + so.mass)
  }
  
  def getHitbox =
    new Hitbox((radius * 2).toInt, (radius * 2).toInt,(radius * 2).toInt, this)
  
  /**Visual representation of the speed vector*/
  private[planets] def tangentLine() {
    val mult = 10f
    val vec = state.vel multiply mult minimum (radius + 3) limit (radius + 12)
    vec.draw(t)
    vec.reverse.draw(t)
  }
  /**Visual representation of the 
   * normal acceleration vector*/
  private[planets] def normalAcceleration() {
    val a = aV limit ( 2 * radius ) minimum ( radius + 5 ) 
    a.draw(t)
  }
  /**Set a new location for this spaceobject*/
  private[planets] def changeLocation(newLocation: Vector3D) = state.pos = newLocation
  
}

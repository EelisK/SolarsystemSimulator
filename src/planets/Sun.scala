package planets

import processing.core.{PApplet,PConstants}
import physics._
import simIO.SatelliteNameGenerator

class Sun(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 1.989e30, velocity, t) {
  
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 0d
  val fileName = "src/images/sun.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Sun"
  val radius   = 30d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  /*Overriden because there is no need to display vectors
   * on the sun, since it most likely doesnt even move*/
  override def display(tangent: Boolean) {
    val l = getScaledLocation
    t.ambientLight(250,250,250,l.i,l.j,l.k)
    super.display(false)
  }
}

class Earth (location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 5.972e24, velocity, t) {
  
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 7.292115053925690e-05
  val fileName = "src/images/earth.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Earth"
  val radius   = 5.5d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Moon (location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 7.34767309e22,velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 2.43e-7
  val fileName = "src/images/moon.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Moon"
  val radius   = 3d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
}

class Satellite(location: Vector3D, velocity: Vector3D, mass: Double)(implicit t: PApplet) 
      extends SpaceObject(location, mass, velocity, t) {
  
  private val index    = SatelliteNameGenerator.getName() //get the satellites name from a file
  private var tracking = false
  private var selected = false
  /*stores the previous locations 
   * of this satellite in case tracking is true*/
  private val previous = collection.mutable.Buffer[(Vector3D, Double)]()
  def this(location: Vector3D, mass: Double)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0), mass)
  }
  /*Random angular velocity*/
  val angularVelocity = util.Random.nextDouble() / 1e5
  val fileName = "src/images/satellite.png"
  val icon     = t.loadImage(fileName)
  val name     = s"Satellite $index"
  val radius   = 2d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
  override def display(tangent: Boolean) = {
    t.pushMatrix()
    val l = getScaledLocation
    t.translate(l.i, l.j, l.k)
    val width  = 2
    val height = 3
    val depth  = 2
    t.fill(255)
    if(tangent) {tangentLine();normalAcceleration()}
    t.fill(188, 198, 204)
    t.rotateY(getAngle.toDegrees)
    /*Set the color to a more visible one
     * in case the satellite is being tracked
     * or observed*/
    if(tracking || selected) 
      t.stroke(20, 255, 20)
    else 
      t.stroke(255)
    t.box(width, height, depth)
    t.popMatrix()
    /**Displays the previous recorded locations of this satellite*/
    def innerDisplay(location: Vector3D, angle: Double) {
      t.pushMatrix()
      t.translate(location.i, location.j, location.k)
      t.stroke(20, 255, 20)
      t.rotateY(angle.toDegrees)
      t.box(width, height, depth)
      t.popMatrix()
    }
    if(tracking) {
      previous.foreach(x => innerDisplay(x._1, x._2))
    }
  }
  /*In addition to updating the state
   * a satellites update method can track the
   * satellites previous locations*/
  override def update(ps: Array[SpaceObject], dt: Double): Unit = {
    super.update(ps, dt)
    if(tracking) {
      if(previous.isEmpty || 
          previous.last._1.sub(getScaledLocation).magnitude > radius * 2)
        saveLocation()
      /*In case the collection 
       * starts to get too large*/
      if(previous.size > 2000) {previous-=previous.head}
    } else if(!tracking && previous.size != 0) {
      previous.clear()
    }
  }
  
  def select() {selected = true}
  
  def deSelect() {selected = false}
  
  def track() {tracking = true}
  
  def untrack() {tracking = false}
  
  def locationTrack() {tracking = !tracking}
  
  /**Save the current location for later use*/
  private def saveLocation() {previous += getScaledLocation -> getAngle}
  
  override def toString() = name
  
}

class Saturn (location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 5.68319e26, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 1.636246173744684e-04
  val fileName = "src/images/saturn.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Saturn"
  val radius   = 11d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
}

class Pluto(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,1.30900e22, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = -1.295641039282477e-05
  val fileName = "src/images/pluto.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Pluto"
  val radius   = 2.9d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Venus(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,4.86732e24, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 3.23e-7
  val fileName = "src/images/venus.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Venus"
  val radius   = 5.5d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Neptune(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,1.024e26, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 1.083382527619075e-04
  val fileName = "src/images/neptune.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Neptune"
  val radius   = 8.5d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Uranus(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,8.68103e25, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = -1.041365902144588e-04
  val fileName = "src/images/uranus.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Uranus"
  val radius   = 8.45d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Mercury(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,3.285e23, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 1.240013441242619e-06
  val fileName = "src/images/mercury.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Mercury"
  val radius   = 1.9d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

class Jupiter(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 1.898e27,velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 1.773408215404907e-04
  val fileName = "src/images/jupiter.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Jupiter"
  val radius   = 13.4d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
}

class Mars(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location, 0.64171e24,velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 7.088218127178316e-05
  val fileName = "src/images/mars.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Mars"
  val radius   = 5.0d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
}

class Meteorite(location: Vector3D, velocity: Vector3D, mass: Double)(implicit t: PApplet) 
      extends SpaceObject(location, mass,velocity, t) {
  def this(location: Vector3D, mass: Double)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0), mass)
  }
  val angularVelocity = util.Random.nextDouble() / 3e3
  val fileName = "src/images/meteorite.png"
  val icon     = t.loadImage(fileName)
  val name     = "Meteorite"
  val radius   = math.min(util.Random.nextDouble * 3, 1)
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
  
}

class BlackHole(location: Vector3D, velocity: Vector3D)(implicit t: PApplet) 
      extends SpaceObject(location,6.9615E30, velocity, t) {
  def this(location: Vector3D)(implicit t: PApplet) {
    this(location, new Vector3D(0,0,0))
  }
  val angularVelocity = 0d
  val fileName = "src/images/blackhole.jpg"
  val icon     = t.loadImage(fileName)
  val name     = "Black hole"
  val radius   = 10d
  val globe    = t.createShape(PConstants.SPHERE, radius)
  globe.setTexture(icon)
}

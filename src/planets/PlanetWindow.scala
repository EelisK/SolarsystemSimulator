package planets

import processing.core._
import ui._
import physics._
import simIO.SimFileReader
import scala.math._
import controlP5._
import peasy.PeasyCam

class PlanetWindow extends PApplet {
  
  self: PlanetWindow =>

  /*Rather than givin 'this'
   * as a parameter for planets and 
   * such, we give it implicitly*/
  private implicit val window = self
  
  /*Some variables need to be 
   * initialized in the setup 
   * method for the program to work*/
  private var console: Console            = _;
  /*All the spaceobjects that are contained
   * within the simulation*/
  private var planets: Array[SpaceObject] = _;
  /*Camera for controlling the scene*/
  private var cam: PeasyCam               = _;
  /*Controlpanel that contains all
   * the gui elements*/
  private var cp5: ControlP5              = _;
  /*Speedslider used to set the timestep*/
  private var speedSlider: Slider         = _;
  /*Background image*/
  private var bg: PImage                  = _;
  
  /*Button names stored in 
   * easy-to-use values*/
  private val arrowLeft  = "\u21E6"
  private val arrowRight = "\u21E8"
  /*This variable helps
   * determining the timestep
   * of this simulation*/
  private var then       = System.nanoTime()
  /*Current camera targets index*/
  private var target     = 0
  /*Should the vectors be
   * displayed or not*/
  private var tangent    = true
  /*Wether collisions should
   * be annotated or not*/
  private var collisions = false
  
  /*Used as an origin for 
   * the lights in the simulation*/
  private lazy val sun =
    planets.find(_.isInstanceOf[Sun])
  /*Used for determining
   * the lighting*/
  private lazy val containsSun = 
    sun.isDefined
  
  /*Avoiding thousands of 
   * toFloat method calls*/
  implicit def toFloat(d: Double) = d.toFloat
  
  override def setup() = {
    background(0)
    fill(255, 255, 0)
    strokeWeight(2f)
    cam = new PeasyCam(this, 1000)
    cam.setMaximumDistance(8000)
    cam.setMinimumDistance(100)
    cam.setResetOnDoubleClick(false)
    cam.rotateY(Pi / 2)
    cam.rotateZ(Pi / 2)
    bg  = loadImage("images/bgimagetransparent.png")
    cp5 = new ControlP5(this)
    bg.resize(width,height)
    cp5.setFont(createFont("", 11f))
    /*Neutral color*/
    cp5.setColorBackground(color(100,149,237))
    /*Hover color*/
    cp5.setColorForeground(color(70,130,180))
    /*Slider hover color*/
    cp5.setColorActive(color(60,120,170))
    /*autodraw to false so the gui 
     * can be displayed without it rotating*/
    cp5.setAutoDraw(false)
    val btnWidth = 120//unknown,x,y,width,height
    cp5.addButton("Save simulation",1, 30, 30, btnWidth, btnWidth/2)
    cp5.addButton("Pause", 1, btnWidth + 40, 30, btnWidth, btnWidth/2)
    cp5.addButton("Vectors", 1, btnWidth * 2 + 50, 30, btnWidth, btnWidth/2)
    cp5.addButton("Collision annotation", 1, btnWidth * 3 + 60, 30, btnWidth + 5, btnWidth/2)
    cp5.addButton("Restart simulation", 1, btnWidth * 4 + 75, 30, btnWidth, btnWidth/2)
    cp5.addButton("Exit", 1, width - btnWidth - 50, height - btnWidth/2 - 30, btnWidth, btnWidth/2)
    /*<-*/
    cp5.addButton(arrowLeft, 1, width - 151, 351, 50, 25)
    /*->*/
    cp5.addButton(arrowRight, 1, width - 100, 351, 50, 25)
    cp5.addButton("change", 1, width - 100, 377, 50, 25)
    cp5.addButton("lock", 1, width - 151, 377, 50, 25)
    cp5.addButton("track", 1, width - 202, 351, 50, 25)
    cp5.addButton("reset", 1, width - 202, 377, 50, 25)
    val c = color(105, 105, 105)
    cp5.getController(arrowLeft).setColorBackground(c)
    cp5.getController(arrowRight).setColorBackground(c)
    cp5.getController("change").setColorBackground(c)
    cp5.getController("lock").setColorBackground(c)
    cp5.getController("track").setColorBackground(c)
    cp5.getController("reset").setColorBackground(c)
    speedSlider = cp5.addSlider("1000 x normal speed",
        -10000f, 10000f, //min, max
        1.44f, 50, //start, x
        130, 30, //y, width
        height*13/17)//height
    /*PShape takes these
     * parameters and
     * the cannot be changed
     * after creation*/
    noStroke()
    /*255 as a background
     * color for the texture*/
    fill(255)
    planets = SimFileReader.readFile()
    console = new Console(planets)
  }
  
  override def draw() = {
    pushMatrix()
    /*background to 0 just in case*/
    background(0)
    /*because bg is somewhat transparent*/
    if(math.abs(width - bg.width) > 0 || math.abs(height - bg.height) > 0)
      bg.resize(width, height)
    background(bg)
    /*Light coming from 
     * the origin/sun*/
    if(containsSun) {
      val l = sun.get.getScaledLocation
      pointLight(255,255,255,l.i,l.j,l.k)
    }
    /*Light fades as it goes further
     * away from the origin*/
    lightFalloff(1.0, 0.001, 0.0)
    update()
    gui()
    popMatrix()
  }
  
  override def settings() = {
    /*Set the size of the app depending 
     * on the users screen size*/
    val s = java.awt.Toolkit.getDefaultToolkit().getScreenSize()
    size(s.width - 150, s.height - 150, PConstants.P3D)
  }
  
  override def keyPressed() = {
    if(keyEvent.getKey == ' ') {
      lockTarget(planets(target))
    }
  }
  
  /**Method for using the 
   * control panel buttons*/
  def controlEvent(e: ControlEvent) = {
    val name = e.getName
    if(name == "Save simulation")
      SimFileReader.saveSimulation(planets)
    else if(name == "Pause")
      speedSlider.setValue(0f)
    else if(name == "Exit")
      exit()
    else if(name == arrowLeft)
      console.move(-1)
    else if(name == arrowRight)
      console.move(1)
    else if(name == "Vectors")
      tangent = !tangent
    else if(name == "Restart simulation") {
      noStroke()
      fill(255)
      planets = SimFileReader.readFile()
      if(containsSun) {
        val loc = sun.get.getScaledLocation
        cam.lookAt(loc.i,loc.j,loc.k, 1000d)
      } else {
        cam.lookAt(0, 0, 0, 1000d)
      }
      cam.setRotations(0, 0, 0)
    }
    else if(name == "lock")
      lockTarget(planets(target))
    else if(name == "change")
      changeTarget()
    else if(name == "reset") {
      target = 0
      val loc = planets(target).getScaledLocation
      cam.lookAt(loc.i, loc.j, loc.k, 1000d)
      cam.setRotations(0, 0, 0)
    }
    else if(name == "Collision annotation")
      collisions = !collisions
    else if(name == "track")
      if(console.tracking.isDefined)
        console.tracking.get.locationTrack()
  }
  
  /**Displaying of planets
   * and updating their
   * positions happens here*/
  private def update() = {
    val now = System.nanoTime()
    /*Timestep is 1000 times the speedsliders value
     * multiplied by the systems delta time*/
    val dt = 1000 * speedSlider.getValue * (now - then) / 1e9
    planets.foreach(_.display(tangent))
    /*loop size is determined
     * by the size of the time 
     * step*/
    val times = 50
    /*Loop increases accuracy
     * in calculations => smaller
     * steps == smaller error*/
    for(i <- 1 to times){
      planets.foreach(_.update(planets, dt / times))
    }
    for ( i <- 0 until planets.size ) {
      /*Inner loop keeps decreasing on
       * every iteration to make it faster
       * and avoid things like collision 
       * with self*/
      for ( j <- i + 1 until planets.size ) {
        /*Checks collision and deals with it*/
        if(planets(i).intersects(planets(j))) {
          planets(i).dealWithCollision(planets(j))
          /*Annotation should only come
           * if user has not disabled it*/
          if(collisions) {
            /*Zoom at the smaller object*/
            val p = Array(planets(i), planets(j)).minBy(_.mass)
            val loc = p.getScaledLocation
            cam.lookAt(loc.i, loc.j, loc.k, p.radius * 3)
          }
        }
      }
    }
    then = now
  }
  
  /**Lock the camera to the selected target*/
  private def lockTarget(o: SpaceObject): Unit = {
    val loc = o.getScaledLocation
    cam.lookAt(loc.i, loc.j, loc.k, 350d)
  }
  
  /**Changes the target to the next
   * object that is not a satellite*/
  private def changeTarget(): Unit = {
    target = (target + 1) % planets.size
    if(planets(target).isInstanceOf[Satellite] && 
        planets.exists(!_.isInstanceOf[Satellite]))
      changeTarget()
    lockTarget(planets(target))
  }
  
  /**Displays the gui 
   * components on the screen*/
  private def gui(): Unit = {
    noLights()
    camera()
    cp5.draw()
    console.display()
    textSize(9f)
    text(s"Collision annotations enabled: $collisions", 10, height - 10)
    text(s"Display vectors: $tangent", 10, height - 20)
  }
  
  /**Returns the current 
   * camera target*/
  def getTarget() = planets(target)
  
  /**Returns the current 
   * update rate of the simulation*/
  def updateRate() = speedSlider.getValue * 1000
  
}
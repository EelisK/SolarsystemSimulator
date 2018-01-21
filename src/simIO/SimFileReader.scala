package simIO

import planets._
import planets.constants._
import physics._
import java.io.{File, FileReader, BufferedReader,PrintWriter}
import processing.core.PApplet

object SimFileReader {
  
  
  /**Get all of the SpaceObjects
   * from the designated file*/
  def readFile[A <: PApplet]()(implicit t: A): Array[SpaceObject] = {
    /*The source file*/
    val source = new File("src/simIO/solarsystem.txt")
    /**Trims the line readable for a machine*/
    def trimLine(l: String): String =
      l.filter(x => x != ' ' && x != '\t').toLowerCase()
    /**Separates the header name and planet name*/
    def trimHeader(s: String): (String, String) = {
      val header = s.split(":")
      (header(0), header(1))
    }
    /**Tuple where the first index contains
     * the datas name and secont index 
     * contains the data itself*/
    def trimData(s: String): (String, String) = {
      val data1 = s.takeWhile(x => x != '[' && x != '(')
      val data2 = s.drop(data1.size)
      (data1, data2)
    }
    /**Returns a tuple of three Double values
     * that are given to a Vector3D object as 
     * parameters*/
    def trimVector(s: String): Vector3D = {
      try {
        val line = s.filter(x => x != '[' && x != ']' && x != '(' && x != ')').split(',')
        val values = if(line.size > 1) line.map(_.toDouble) else Array[Double]()
        /*Just in case user has inserted 
         * 2 dimensional coordinates in the
         * simulation file*/
        if(values.size == 3)
          Vector3D(values(0), values(1), values(2))
        else if(values.size == 2)
          Vector3D(values(0), values(1))
        else
          Vector3D()
      } catch {
        /*In case something goes wrong*/
        case _: Throwable => 
          throw new CorruptedSimFileException("File contains corrupted vector(s)\nCannot determine objects location/velocity")
      }
    }
    def isHeader(l: String): Boolean = l.startsWith("body")
    def isData(l: String): Boolean = l.startsWith(":")
    var mass = 1000d
    /*The function that gives the
     * returned buffer*/
    def readAll() = {
      var array  = Array[SpaceObject]()
      val input  = new FileReader(source)
      val reader = new BufferedReader(input)
      val lines  = reader.lines                      //get lines from file
                         .toArray()                  //store the lines in an array
                         .map(_.toString)            //turn them in to string type
                         .map(_.takeWhile(_ != '#')) //remove comments
                         .map(trimLine)              //remove all spaces and tabs
                         .filter(_ != "")            //remove empty lines
      reader.close() //close the data stream
      input.close()
      
      var velocity = Vector3D(0, 0, 0)
      var location: Vector3D = null
      var planetType = ""
      
      for ( line <- lines ) {
        
        /*First check if the line
         * is a header*/
        if(isHeader(line)) {
          /*If this is not the first header, 
           * add a planet to the array
           * accordingly and reset the vectors.
           * */
          if(planetType != "") {
            array  ++= Vector(getPlanet(planetType, location, velocity))
            velocity = Vector3D(0, 0, 0)
            location = null
            mass     = 1000d
          }
          /*Set a new planet type*/
          planetType = trimHeader(line)._2
        }
        else if(isData(line)) {
          if(line.startsWith(":mass")) {
            mass = line.drop(5).toDouble
          } else {
            /*(dataName, data)*/
            val d = trimData(line)
            val vector = trimVector(d._2)
            d._1 match {
              case ":velocity" => velocity = vector multiply 1e3
              case ":location" => location = vector multiply 1e3 * 1e6
              case _           => {
                Console.out.println(Console.RED + "Unknown data " + d._1 + Console.RESET)
              }
            }
          }
        }
      }
      /*In case no planets were added*/
      if(planetType != "") {
        val lastPlanet = getPlanet(planetType, location, velocity)
        array ++= Vector(lastPlanet)
      } else {
        throw new CorruptedSimFileException("Simulation does not contain any objects")
      }
      /*Check that no planets are in the same lockation*/
      for(i <- 0 until array.size;j <- i + 1 until array.size) {
        if(array(i).getLocation == array(j).getLocation)
          throw new CorruptedSimFileException("Cannot add object so the same location")
      }
      
      array
    }
    
    def getPlanet(s: String, location: Vector3D, velocity: Vector3D) = {
      s match {
        case "sun"       => new Sun(location, velocity)
        case "mercury"   => new Mercury(location, velocity)
        case "venus"     => new Venus(location, velocity)
        case "earth"     => new Earth(location, velocity)
        case "moon"      => new Moon(location, velocity)
        case "satellite" => new Satellite(location, velocity, mass)
        case "mars"      => new Mars(location, velocity)
        case "jupiter"   => new Jupiter(location, velocity)
        case "saturn"    => new Saturn(location, velocity)
        case "uranus"    => new Uranus(location, velocity)
        case "neptune"   => new Neptune(location, velocity)
        case "pluto"     => new Pluto(location, velocity)
        case "meteorite" => new Meteorite(location, velocity, mass)
        case "blackhole" => new BlackHole(location, velocity)
        case _ =>
          throw new CorruptedSimFileException(s"Unknown planet $s")
      }
    }
    
    try {
      /*Where the magic happens*/
      return readAll
      
    } catch {
      /*In case a correct error was already thrown*/
      case details: CorruptedSimFileException => throw details
      case _: Throwable => 
        throw new CorruptedSimFileException("Unknown error\nRead solarsystem.txt for more info on creating planets")
    }
  }
  
  
  /**Saves the simulation 
   * in the designated file*/
  def saveSimulation(planets: Array[SpaceObject]) = {
    /**Information about the file 
     * to be written on the overwritten
     * simulation file as well*/
    val infoFile = new File("src/simIO/.info")
    var input  = new FileReader(infoFile)
    var reader = new BufferedReader(input)
    val info = reader.lines().toArray().map(_.toString)
    val writer = new PrintWriter("src/simIO/solarsystem.txt")
    /*Write info about the simulation 
     * file and how to edit and 
     * make additions to it*/
    info.foreach(writer.println)
    for(planet <- planets) {
      val planetType = "body: " + getPlanetType(planet)    
      /*toString method of vectors puts 
      * them in the default persentable form*/
      val speed = "\t:velocity\t" + planet.getVelocity.div(1e3).toString
      val location = "\t:location\t" + planet.getLocation.div(1e9).toString
      writer.println(planetType)
      writer.println(location)
      writer.println(speed)
      /*Write an empty line for readability*/
      writer.println()
    }
    def getPlanetType(planet: SpaceObject) = {
      planet match {
        case sun: Sun             => "Sun"
        case mercury: Mercury     => "Mercury"
        case venus: Venus         => "Venus"
        case earth: Earth         => "Earth"
        case moon: Moon           => "Moon"
        case satellite: Satellite => "Satellite"
        case mars: Mars           => "Mars"
        case jupiter: Jupiter     => "Jupiter"
        case saturn: Saturn       => "Saturn"
        case uranus: Uranus       => "Uranus"
        case neptune: Neptune     => "Neptune"
        case pluto: Pluto         => "Pluto"
        case meteorite: Meteorite => "Meteorite"
        case _                    => "Unknown"
      }
    }
    writer.close()
  }
  
  
  
}
package simIO

import java.io.{File, FileReader, BufferedReader}

object SatelliteNameGenerator {
  
  /*Stores the previously used satellite names*/
  private var namesUsed = Vector[String]()
  private var emergencyIndex = 0
  
  /**Get a real satellite name from a designated file.
   * If all the names are used, returns just an index*/
  def getName(): String = {
    val file = new File("src/simIO/.satellitenames")
    val reader = new FileReader(file)
    val bufferedReader = new BufferedReader(reader)
    try {
      val lines = bufferedReader.lines().iterator()
      var name = lines.next()
      if(emergencyIndex > 0) {
        emergencyIndex += 1
        return s"$emergencyIndex"
      }
      while(lines.hasNext() && namesUsed.contains(name)) {
        name = lines.next()
      }
      if(namesUsed.contains(name)) {
        emergencyIndex += 1
        return s"$emergencyIndex"
      }
      namesUsed ++= Vector(name)
      name
    } finally {
      reader.close()
      bufferedReader.close()
    }
  }
  
}
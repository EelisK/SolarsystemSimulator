package planets

/**Contains commonly used values*/
package object constants {
  
  /**Gravitational constant used for 
   * calculating gravitational forces
   * between objects*/
  final val gravitationalConstant = 6.67408e-11
  
  /**Aka astronomical unit.
   * Equal to 149.597871e9 meters*/
  final val AU = 149.597871e9
  
  /**Light speed in [m/s]. Can be used for
   * limiting velocities*/
  final val lightSpeed = 299792458d
  
  /**Value for setting relative distances*/
  final val metersPerPixel: Double = 3e8
  
}
package physics

import scala.math._
import processing.core.PApplet
import processing.core.PConstants


class Vector3D(val i: Double, val j: Double, val k: Double = 0) {
  
  /**Dot product of two vectors*/
  def dot(vector: Vector3D): Double = i * vector.i + j * vector.j + k * vector.k
  
  def add(vector: Vector3D): Vector3D = new Vector3D( i + vector.i, j + vector.j , k + vector.k)
  
  def +(vector: Vector3D): Vector3D = add(vector)
  
  /*Returns a vector with the length of 1*/
  def normalize: Vector3D = new Vector3D( i / magnitude, j / magnitude , k / magnitude)
  
  /**Angle in radians*/
  def angleBetween(vector: Vector3D): Double = 
    acos(dot(vector).toDouble / (magnitude * vector.magnitude))
    
  /**Returns the angle between
   * the y -axis and the vector*/
  def angle(): Double = {
    val n = 1
    val r = magnitude
    2 * ( Pi * n * atan((k - r) / sqrt(pow(r, 2) - pow(k, 2))))
  }
  
  /*Returns a vectors wich has atleast
   * atleast the length of the minimum size*/
  def minimum(size: Double): Vector3D = {
    if(magnitude > size)
      this
    else
      normalize.multiply(size)
  }
  
  /**Size of the vector*/
  def magnitude: Double = sqrt(pow(i.toDouble, 2) + pow(j.toDouble, 2) + pow(k.toDouble, 2))
  
  def sub(vector: Vector3D): Vector3D = new Vector3D(i - vector.i, j - vector.j, k - vector.k)
  
  def div(by: Double): Vector3D = new Vector3D( i / by, j / by , k / by)
  
  def multiply(by: Double) = new Vector3D( i * by, j * by , k * by)
  
  def *(by: Double) = this multiply by
  
  /**Limit the vectors size by a
   * certain amount*/
  def limit(maxLength: Double) = {
    if (magnitude > maxLength)
      normalize multiply maxLength
    else
      this
  }
  
  def reverse = new Vector3D(-i, -j, -k)
  
  def cross(vector: Vector3D): Vector3D = {
    new Vector3D(j * vector.k - k * vector.j, 
                 k * vector.i - i * vector.k, 
                 i * vector.j - j * vector.i)
  }
  
  /**Draws this vector on a subclass
   * of PApplet. Assumes the origin is
   * [0, 0, 0]*/
  def draw[A <: PApplet](canvas: A) = {
    canvas.strokeWeight(1f)
    canvas.stroke(255)
    canvas.fill(255)
    canvas.line(0, 0, 0, i.toFloat, j.toFloat, k.toFloat)
  }
  
  def ==(another: Vector3D): Boolean =
    i == another.i && j == another.j && k == another.k
    
  def !=(another: Vector3D): Boolean = 
    i != another.i || j != another.j || k != another.k
  
  override def toString() = s"[$i, $j, $k]"
  
}

/**Companion object
 * of class Vector3D*/
object Vector3D {
  
  def apply() = new Vector3D(0, 0, 0)
  
  def apply(i: Double, j: Double) = new Vector3D(i, j, 0)
  
  def apply(i: Double, j: Double, k: Double) = new Vector3D(i, j, k)
  
  def dot(vector1: Vector3D, vector2: Vector3D): Double = vector1.dot(vector2)
  
  def cross(vector1: Vector3D, vector2: Vector3D): Vector3D = vector1.cross(vector2)
  
  def add(vector1: Vector3D, vector2: Vector3D): Vector3D = vector1.add(vector2)
  
  def normalize(vector: Vector3D): Vector3D = vector.normalize
  
  def angleBetween(vector1: Vector3D, vector2: Vector3D): Double = vector1.angleBetween(vector2)
  
  def magnitude(vector: Vector3D): Double = vector.magnitude
  
  def sub(vector1: Vector3D, vector2: Vector3D): Vector3D = vector1.sub(vector2)
  
  def div(vector: Vector3D, by: Double): Vector3D = vector.div(by)
  
  def multiply(v1: Vector3D, by: Double): Vector3D = v1.multiply(by)
  
  /**Distance between two location vectors*/
  def distance(v1: Vector3D, v2: Vector3D): Double = 
    sqrt(pow((v1.i - v2.i).toDouble, 2) + pow((v1.j - v2.j).toDouble, 2) + pow((v1.k - v2.k).toDouble, 2))
  
}
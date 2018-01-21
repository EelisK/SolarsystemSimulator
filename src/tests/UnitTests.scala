package tests

import org.junit.Test
import org.junit.Assert._
import physics._
import planets._

class UnitTests {
  
  @Test def testVectors() {
    val v1 = Vector3D(1,0,0)
    assertTrue("Vectors magnitude should be 1.0", v1.magnitude == 1.0)
    val v2 = Vector3D(1,1,0)
    assertTrue("Vectors magnitude should be sqrt(2)", v2.magnitude == math.sqrt(2))
  }
  
  @Test def testState() {
    val s = State()
    assertTrue(s.pos.magnitude == 0 && s.vel.magnitude == 0)
    val d = Derivate()
    s.add(d)
    assertTrue(s.pos.magnitude == 0 && s.vel.magnitude == 0)
    d.acceleration = Vector3D(1,0)
    s.add(d)
    assertTrue(s.vel.magnitude == 1)
  }
  
  @Test def testVectorCross() {
    val a = Vector3D(18,9,4)
    val b = Vector3D(32, 2, 9)
    val cross = a cross b
    val answer = Vector3D(73, -34, -252)
    assertTrue(s"Cross product should be $answer, but was $cross", cross == answer)
  }
  
  @Test def testSub() {
    val v1 = Vector3D(100, 10, 0)
    val v2 = Vector3D(100, 10, 0)
    assertTrue(s"Vectors $v1 and $v2 subtracted should result in a zero vector", 
        v1.sub(v2) == Vector3D())
    val v3 = v2.reverse
    assertTrue(s"Reversing vector $v2 and addin it to $v1 should result in a zero vector", 
        v1.add(v3) == Vector3D())
  }
  
  /**An error can be seen with
   * large enough vectors*/
  @Test def testLimitVector() {
    val v = Vector3D(1e9, 23213, 32)
    val v1 = v.limit(100).magnitude
    assertTrue("Size should be 100 but was " + v1, 
        v1 >= 99.9999999999 && v1 <= 100.0000000001)
  }
  
  @Test def testMinimumVector() {
    val v = Vector3D(0.1, 0, 0)
    val size = 10
    val s = v.minimum(10).magnitude
    assertTrue("Magnitude should be "+size+" but was " + s, s == size)
  }
  
  @Test def testNormalize() {
    val v1 = Vector3D(0.001, -0.00032133, 0)
    val s1 = v1.normalize.magnitude
    assertTrue(s"Size of normal vector should be 1 but was $s1", s1 == 1)
    val v2 = Vector3D(0, 99123, 0)
    val s2 = v2.normalize.magnitude
    assertTrue(s"Size of normal vector should be 1 but was $s2", s2 == 1)
    val v3 = Vector3D(0, 0, 1231)
    val s3 = v3.normalize.magnitude
    assertTrue(s"Size of normal vector should be 1 but was $s3", s3 == 1)
  }
  
  
}
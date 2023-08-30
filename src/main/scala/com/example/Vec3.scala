package com.example.vec3

import cats.effect.IO

case class Vec3(x: Double, y: Double, z: Double)

trait Vec3Ops[A]:
  extension (c: Double) def *:(b: A): A
  extension (self: A)
    def -(b: A): A
    def +(b: A): A
    def *(c: Double): A
    def *(b: A): A
    def /(c: Double): A
    def lengthSquared: Double
    def length: Double
    def dot(b: A): Double
    def cross(b: A): A
    def unitVector: A
object Vec3:
  extension (self: Vec3)
    def unary_- : Vec3 = Vec3(-self.x, -self.y, -self.z)
  given vec3Ops: Vec3Ops[Vec3] with
    extension (c: Double) def *:(b: Vec3): Vec3 = b * c
    extension (self: Vec3)
      def -(b: Vec3): Vec3 = Vec3(self.x - b.x, self.y - b.y, self.z - b.z)
      def +(b: Vec3): Vec3 = Vec3(self.x + b.x, self.y + b.y, self.z + b.z)
      def *(c: Double): Vec3 = Vec3(self.x * c, self.y * c, self.z * c)
      def *(b: Vec3): Vec3 = Vec3(self.x * b.x, self.y * b.y, self.z * b.z)
      def /(c: Double): Vec3 = Vec3(self.x / c, self.y / c, self.z / c)
      def lengthSquared: Double =
        self.x * self.x + self.y * self.y + self.z * self.z
      def length: Double = math.sqrt(self.lengthSquared)
      def dot(b: Vec3): Double = self.x * b.x + self.y * b.y + self.z * b.z
      def cross(b: Vec3): Vec3 = Vec3(
        self.y * b.z - self.z * b.y,
        self.z * b.x - self.x * b.z,
        self.x * b.y - self.y * b.x
      )
      def unitVector: Vec3 = self / self.length

inline def <<(v: Vec3): IO[Unit] = IO {
  println(s"${v.x} ${v.y} ${v.z}")
}

// opaque type Point3 = Vec3

// object Point3:
//   def apply(v: Vec3): Point3 = v
//   def apply(x: Double, y: Double, z: Double): Point3 = Vec3(x, y, z)
//   extension (self: Point3) def value: Vec3 = self

//   given (using ops: Vec3Ops[Vec3]): Vec3Ops[Point3] with
//     extension (c: Double) def *:(b: Point3): Point3 = ops.*(b)(c)
//     extension (self: Point3)
//       def -(b: Point3): Point3 = ops.-(self)(b)
//       def +(b: Point3): Point3 = ops.+(self)(b)
//       def *(c: Double): Point3 = ops.*(self)(c)
//       def *(b: Point3): Point3 = ops.*(self)(b)
//       def /(c: Double): Point3 = ops./(self)(c)
//       def lengthSquared: Double = ops.lengthSquared(self)
//       def length: Double = ops.length(self)
//       def dot(b: Point3): Double = ops.dot(self)(b)
//       def cross(b: Point3): Point3 = ops.cross(self)(b)
//       def unitVector: Point3 = ops.unitVector(self)

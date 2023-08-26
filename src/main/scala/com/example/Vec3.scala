package com.example

import cats.effect.IO

trait Vec3:
  def x: Double
  def y: Double
  def z: Double

  def init(x: Double, y: Double, z: Double): Vec3

  def lengthSquared = x * x + y * y + z * z

  extension (self: Vec3)
    def - : Vec3 = init(-x, -y, -z)

    inline def +(v: Vec3): Vec3 = init(self.x + v.x, self.y + v.y, self.z + v.z)

    inline infix def *(c: Double): Vec3 = init(self.x * c, self.y * c, self.z * c)

    inline infix def *(v: Vec3): Vec3 = init(self.x * v.x, self.y * v.y, self.z * v.z)

    inline def /(c: Double): Vec3 = self * (1 / c)

    def length = Math.sqrt(lengthSquared)

    def dot(v: Vec3): Double = self.x * v.x + self.y * v.y + self.z * v.z

    def cross(v: Vec3): Vec3 = init(
      self.y * v.z - self.z * v.y,
      self.z * v.x - self.x * v.z,
      self.x * v.y - self.y * v.x
    )

    def unitVector: Vec3 = self / length.apply(self)

  infix def *(d: Double, v: Vec3): Vec3 = v * d

  infix def /(d: Double, v: Vec3): Vec3 = v * (1 / d)

inline def <<(v: Vec3): IO[Unit] = IO {
  println(s"${v.x} ${v.y} ${v.z}")
}

case class Point3(a: Double, b: Double, c: Double) extends Vec3:
  self =>
  def init(x: Double, y: Double, z: Double): Vec3 = Point3(x, y, z)

  def x = self.a
  def y = self.b
  def z = self.c


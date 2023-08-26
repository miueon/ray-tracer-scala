package com.example

import cats.effect.IO

case class Color(a: Double, b: Double, c: Double) extends Vec3:
  self =>
  def init(x: Double, y: Double, z: Double): Vec3 = Color(x, y, z)

  def x = self.a
  def y = self.b
  def z = self.c

object Color:
  val base = 255.999

  extension (self: Color)
    def r = self.x * base
    def g = self.y * base
    def b = self.z * base

    def writeColor: IO[Unit] = IO {
      println(s"${self.r} ${self.g} ${self.b}")
    }

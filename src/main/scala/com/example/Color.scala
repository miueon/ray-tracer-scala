package com.example

import cats.effect.IO
import com.example.vec3.Vec3
import com.example.vec3.Vec3Ops

opaque type Color = Vec3

object Color:
  val base = 255.99
  def apply(v: Vec3): Color = v
  def apply(x: Double, y: Double, z: Double): Color = Vec3(x, y, z)
  extension (self: Color)
    def r = self.x * base
    def g = self.y * base
    def b = self.z * base

    def value: Vec3 = self
    def writeColor: IO[Unit] = IO {

      println(s"${r} ${g} ${b}")
    }

  given (using ops: Vec3Ops[Vec3]): Vec3Ops[Color] with
    extension (c: Double) def *:(b: Color): Color = ops.*(b)(c)
    extension (self: Color)
      def -(b: Color): Color = ops.-(self)(b)
      def +(b: Color): Color = ops.+(self)(b)
      def *(c: Double): Color = ops.*(self)(c)
      def *(b: Color): Color = ops.*(self)(b)
      def /(c: Double): Color = ops./(self)(c)
      def lengthSquared: Double = ops.lengthSquared(self)
      def length: Double = ops.length(self)
      def dot(b: Color): Double = ops.dot(self)(b)
      def cross(b: Color): Color = ops.cross(self)(b)
      def unitVector: Color = ops.unitVector(self)

package com.example

import cats.effect.IO
import com.example.vec3.Vec3.vec3Ops
import com.example.vec3.Vec3
import com.example.vec3.Vec3Ops

opaque type Color = Vec3

object Color:
  val base = 255.99
  val intensity = Interval(0.0, 0.999)
  def apply(v: Vec3): Color = v
  def apply(x: Double, y: Double, z: Double): Color = Vec3(x, y, z)
  private def colorBased(x: Double) = x * base
  def colorBasedWithGamma(x: Double) = math.sqrt(x) * base
  extension (self: Color)

    def value: Vec3 = self
    def writeColor(samplePerPixel: Int): IO[Unit] = IO {
      val scale = 1.0 / samplePerPixel
      val r = self.x * scale
      val g = self.y * scale
      val b = self.z * scale
      println(
        s"${colorBasedWithGamma(r)} ${colorBasedWithGamma(g)} ${colorBasedWithGamma(b)}"
      )
    }

  given colorVecOps: Vec3Ops[Color] = vec3Ops

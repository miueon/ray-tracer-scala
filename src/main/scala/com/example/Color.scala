package com.example

import cats.effect.IO
import com.example.vec3.Vec3.vec3Ops
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

  given colorVecOps: Vec3Ops[Color] = vec3Ops
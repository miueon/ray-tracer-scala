package com.example

import vec3.*
import vec3.given

case class Ray(origin: Vec3, direction: Vec3)
object Ray:
  extension (r: Ray)(using ops: Vec3Ops[Vec3])
    def at(t: Double): Vec3 = r.origin + t *: r.direction

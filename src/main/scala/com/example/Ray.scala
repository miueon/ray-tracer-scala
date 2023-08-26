package com.example

case class Ray(origin: Point3, direction: Vec3)
object Ray:
  extension (r: Ray)
    def at(t: Double): Point3 = r.origin + r.direction.*(t)
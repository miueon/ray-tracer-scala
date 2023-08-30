package com.example

import com.example.vec3.Vec3

case class HitRecord(p: Vec3, normal: Vec3, t: Double, frontFace: Boolean)
object HitRecord:
  def apply(hitPoint: Vec3, outwardNormal: Vec3, t: Double): HitRecord =
    val frontFace = outwardNormal.dot(hitPoint) < 0
    val normal = if frontFace then outwardNormal else -outwardNormal
    HitRecord(hitPoint, normal, t, frontFace)

trait Hittable[A]:
  extension (self: A)
    def hit(r: Ray, rayTmin: Double, rayTmax: Double): Option[HitRecord]

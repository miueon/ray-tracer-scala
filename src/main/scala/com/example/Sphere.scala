package com.example

import com.example.vec3.Vec3

case class Sphere(center: Vec3, radius: Double)
object Sphere:
  given SphereHittable: Hittable[Sphere] with

    extension (self: Sphere)
      override def hit(
          r: Ray,
          rayTmin: Double,
          rayTmax: Double
      ): Option[HitRecord] =
        val oc = r.origin - self.center
        val a = r.direction.lengthSquared
        val halfB = oc.dot(r.direction)
        val c = oc.lengthSquared - self.radius * self.radius
        val discriminant = halfB * halfB - a * c

        val sqrtd = math.sqrt(discriminant)
        val root1 = (-halfB - sqrtd) / a
        val root2 = (-halfB + sqrtd) / a

        def mkHitRecord(root: Double): Option[HitRecord] =
          val p = r.at(root)
          val outwardNormal = (p - self.center) / self.radius
          Some(HitRecord(p, outwardNormal, root))
        if discriminant < 0 then None
        else if (root1 > rayTmin && root1 < rayTmax) then mkHitRecord(root1)
        else if (root2 > rayTmin && root2 < rayTmax) then mkHitRecord(root2)
        else None

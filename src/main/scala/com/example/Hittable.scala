package com.example.hittable

import com.example.vec3.Vec3
import com.example.Ray

case class HitRecord(p: Vec3, normal: Vec3, t: Double, frontFace: Boolean)
object HitRecord:
  def apply(hitPoint: Vec3, outwardNormal: Vec3, t: Double): HitRecord =
    val frontFace = outwardNormal.dot(hitPoint) < 0
    val normal = if frontFace then outwardNormal else -outwardNormal
    HitRecord(hitPoint, normal, t, frontFace)

trait Hittable[A]:
  extension (self: A)
    def hit(r: Ray, rayTmin: Double, rayTmax: Double): Option[HitRecord]

enum HittableObject:
  case Sphere(center: Vec3, radius: Double)
  case HList(hitables: List[HittableObject])

object HittableObject:
  import HittableObject.{Sphere, HList}
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
  given HListHittable: Hittable[HList] with
    extension (self: HList)
      override def hit(
          r: Ray,
          rayTmin: Double,
          rayTmax: Double
      ): Option[HitRecord] =
        self.hitables.foldLeft(Option.empty[HitRecord]) {
          (acc, hittableObject) =>
            acc match
              case Some(hitRecord) =>
                hittableObject.hit(r, rayTmin, hitRecord.t).orElse(Some(hitRecord))
              case None =>
                hittableObject.hit(r, rayTmin, rayTmax)
        }
  given ObjectHittable: Hittable[HittableObject] with
    extension (self: HittableObject)
      def hit(
          r: Ray,
          rayTmin: Double,
          rayTmax: Double
      ): Option[HitRecord] =
        self match
          case s: Sphere => summon[Hittable[Sphere]].hit(s)(r, rayTmin, rayTmax)
          case h: HList  => summon[Hittable[HList]].hit(h)(r, rayTmin, rayTmax)

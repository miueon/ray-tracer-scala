package com.example.hittable

import com.example.vec3.Vec3
import com.example.Ray
import com.example.Interval
import com.example.Material

case class HitRecord(p: Vec3, normal: Vec3, t: Double, frontFace: Boolean)
object HitRecord:
  def apply(hitPoint: Vec3, outwardNormal: Vec3, t: Double): HitRecord =
    val frontFace = outwardNormal.dot(hitPoint) < 0
    val normal = if frontFace then outwardNormal else -outwardNormal
    HitRecord(hitPoint, normal, t, frontFace)

trait Hittable[A]:
  extension (self: A)
    def hit(r: Ray, rayT: Interval): Option[(HitRecord, Material)]

enum HittableObject:
  case Sphere(center: Vec3, radius: Double, material: Material)
  case HList(hitables: List[HittableObject])

object HittableObject:
  import HittableObject.{Sphere, HList}
  given SphereHittable: Hittable[Sphere] with

    extension (self: Sphere)
      override def hit(
          r: Ray,
          rayT: Interval
      ): Option[(HitRecord, Material)] =
        val oc = r.origin - self.center
        val a = r.direction.lengthSquared
        val halfB = oc.dot(r.direction)
        val c = oc.lengthSquared - self.radius * self.radius
        val discriminant = halfB * halfB - a * c

        val sqrtd = math.sqrt(discriminant)
        val root1 = (-halfB - sqrtd) / a
        val root2 = (-halfB + sqrtd) / a

        def mkHitRecord(root: Double): Option[(HitRecord, Material)] =
          val p = r.at(root)
          val outwardNormal = (p - self.center) / self.radius
          Some((HitRecord(p, outwardNormal, root), self.material))
        if discriminant < 0 then None
        else if (rayT.surrounds(root1)) then mkHitRecord(root1)
        else if (rayT.surrounds(root2)) then mkHitRecord(root2)
        else None
  given HListHittable: Hittable[HList] with
    extension (self: HList)
      override def hit(
          r: Ray,
          rayT: Interval
      ): Option[(HitRecord, Material)] =
        self.hitables.foldLeft(Option.empty[(HitRecord, Material)]) {
          (acc, hittableObject) =>
            acc match
              case Some((hitRecord, material)) =>
                hittableObject
                  .hit(r, Interval(rayT.min, hitRecord.t))
                  .orElse(Some((hitRecord, material)))
              case None =>
                hittableObject.hit(r, rayT)
        }
  given ObjectHittable: Hittable[HittableObject] with
    extension (self: HittableObject)
      def hit(
          r: Ray,
          rayT: Interval
      ): Option[(HitRecord, Material)] =
        self match
          case s: Sphere => summon[Hittable[Sphere]].hit(s)(r, rayT)
          case h: HList  => summon[Hittable[HList]].hit(h)(r, rayT)

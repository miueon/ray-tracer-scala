package com.example

import com.example.hittable.HitRecord
import com.example.vec3.Vec3

enum Material:
  case Lambertian(albedo: Color)
  case Metal(albedo: Color, fuzz: Double)

trait Interact[A]:
  extension (self: A)
    def scatter(rIn: Ray, rec: HitRecord): Option[(Ray, Color)]

object Material:
  given LambertianInteract: Interact[Lambertian] with
    extension (self: Lambertian)
      override def scatter(rIn: Ray, rec: HitRecord): Option[(Ray, Color)] =
        val attemptDirect = rec.normal + Vec3.randomUnitVector()
        val scatterDirection =
          if attemptDirect.nearZero() then rec.normal else attemptDirect
        val scattered = Ray(rec.p, scatterDirection)
        val attenuation = self.albedo
        Some((scattered, attenuation))

  given MetalInteract: Interact[Metal] with
    extension (self: Metal)
      override def scatter(rIn: Ray, rec: HitRecord): Option[(Ray, Color)] =
        val reflected = rIn.direction.unitVector.reflect(rec.normal)
        val scattered =
          Ray(rec.p, reflected + self.fuzz *: Vec3.randomInUnitSphere())
        val attenuation = self.albedo
        if scattered.direction.dot(rec.normal) > 0 then
          Some((scattered, attenuation))
        else None

  given MaterialInteract: Interact[Material] with
    extension (self: Material)
      override def scatter(rIn: Ray, rec: HitRecord): Option[(Ray, Color)] =
        self match
          case l: Lambertian => summon[Interact[Lambertian]].scatter(l)(rIn, rec)
          case m: Metal => summon[Interact[Metal]].scatter(m)(rIn, rec)

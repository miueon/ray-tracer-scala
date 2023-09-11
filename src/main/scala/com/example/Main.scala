package com.example

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.syntax.all
import cats.syntax.all.toTraverseOps
import cats.instances.ListInstances
import com.example.vec3.*
import com.example.vec3.given
import com.example.hittable.HittableObject
import com.example.hittable.Hittable

object Render extends IOApp.Simple:
  val groundMaterial = Material.Lambertian(Color(0.8, 0.8, 0.0))
  val centerMaterial = Material.Lambertian(Color(0.7, 0.3, 0.3))
  val leftMaterial = Material.Metal(Color(0.8, 0.8, 0.8), 0.3)
  val rightMaterial = Material.Metal(Color(0.8, 0.6, 0.2), 1.0)

  val world = HittableObject.HList(
    List(
      HittableObject.Sphere(Vec3(0, 0, -1), 0.5, centerMaterial),
      HittableObject.Sphere(Vec3(0, -100.5, -1), 100, groundMaterial),
      HittableObject.Sphere(Vec3(-1, 0, -1), 0.5, leftMaterial),
      HittableObject.Sphere(Vec3(1, 0, -1), 0.5, rightMaterial)
    )
  )

  def run: IO[Unit] =
    val cam = Camera(16.0 / 9.0, 400, 100, 10)
    cam.render(world)

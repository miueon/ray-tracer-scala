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

  val world = HittableObject.HList(
    List(
      HittableObject.Sphere(Vec3(0, 0, -1), 0.5),
      HittableObject.Sphere(Vec3(0, -100.5, -1), 100)
    )
  )

  def run: IO[Unit] =
    val cam = Camera(16.0 / 9.0, 400)
    cam.render(world)

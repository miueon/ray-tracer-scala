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
  val imageRatio = 16.0 / 9.0
  val imageWidth = 400
  var imageHeight = (imageWidth / imageRatio).toInt
  val focalLength = 1.0
  val viewportHeight = 2.0
  val viewportWidth = (imageWidth.toDouble / imageHeight) * viewportHeight
  val cameraCenter = Vec3(0, 0, 0)

  val viewportU = Vec3(viewportWidth, 0, 0)
  val viewportV = Vec3(0, -viewportHeight, 0)

  val pixelDeltaU = viewportU / imageWidth
  val pixelDeltaV = viewportV / imageHeight

  val viewportUpperLeft = cameraCenter - Vec3(
    0,
    0,
    focalLength
  ) - (viewportU / 2.0) - (viewportV / 2.0)

  val pixel100Loc = viewportUpperLeft + 0.5 *: (pixelDeltaU + pixelDeltaV)

  val world = HittableObject.HList(
    List(
      HittableObject.Sphere(Vec3(0, 0, -1), 0.5),
      HittableObject.Sphere(Vec3(0, -100.5, -1), 100)
    )
  )

  def rayColor(r: Ray, world: HittableObject)(using
      hittable: Hittable[HittableObject]
  ): Color =
    world.hit(r, Interval(0, Double.PositiveInfinity)) match
      case Some(rec) => Color(0.5 *: (rec.normal + Vec3(1, 1, 1)))
      case None =>
        val unitDirection = r.direction.unitVector
        val a = 0.5 * (unitDirection.y + 1.0)
        val color =
          (1.0 - a) *: Color(1.0, 1.0, 1.0) + a *: Color(0.5, 0.7, 1.0)
        color

  def renderPixel(i: Int, j: Int): IO[Unit] =
    val pixelCenter = pixel100Loc + (i *: pixelDeltaU) + (j *: pixelDeltaV)
    val rayDirection = pixelCenter - cameraCenter
    val ray = Ray(cameraCenter, rayDirection)

    val pixelColor = rayColor(ray, world)
    pixelColor.writeColor

  def renderCol(j: Int): IO[Unit] =
    for {
      _ <- printProcess(j)
      _ <- (0 until imageWidth).toList.traverse(i => renderPixel(i, j))
    } yield ()

  def renderImage: IO[Unit] =
    for {
      _ <- (0 until imageHeight).toList.traverse(j => renderCol(j))
    } yield ()

  def printProcess(j: Int): IO[Unit] =
    IO {
      Console.err.print(s"\rScanlines reminaing: ${imageHeight - j}")
      Console.err.flush()
    }

  def header: IO[Unit] = IO {
    println(s"P3\n $imageWidth $imageHeight\n255\n")
  }

  def run: IO[Unit] =
    header *> renderImage *> IO {
      Console.err.println("\n\rDone.")
    }

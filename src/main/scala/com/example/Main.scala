package com.example

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.syntax.all
import cats.syntax.all.toTraverseOps
import cats.instances.ListInstances
import com.example.vec3.*
import com.example.vec3.given

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

  def hitSphere(center: Vec3, radius: Double, ray: Ray): Double =
    val oc = ray.origin - center
    val a = ray.direction.lengthSquared
    val halfB = oc dot ray.direction
    val c = oc.lengthSquared - radius * radius
    val discriminant = halfB * halfB - a * c

    if discriminant < 0 then -1.0
    else (-halfB - math.sqrt(discriminant)) / a

  def rayColor(r: Ray): Color =
    val t = hitSphere(Vec3(0, 0, -1), 0.5, r)
    if t > 0.0 then
      val N = (r.at(t) - Vec3(0, 0, -1)).unitVector
      0.5 *: Color(N.x + 1, N.y + 1, N.z + 1)
    else
      val unitDirection = r.direction.unitVector
      val a = 0.5 * (unitDirection.y + 1.0)
      val color = (1.0 - a) *: Color(1.0, 1.0, 1.0) + a *: Color(0.5, 0.7, 1.0)
      color

  def renderPixel(i: Int, j: Int): IO[Unit] =
    val pixelCenter = pixel100Loc + (i *: pixelDeltaU) + (j *: pixelDeltaV)
    val rayDirection = pixelCenter - cameraCenter
    val ray = Ray(cameraCenter, rayDirection)

    val pixelColor = rayColor(ray)
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

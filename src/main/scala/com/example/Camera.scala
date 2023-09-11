package com.example

import com.example.hittable.HittableObject
import cats.effect.IO
import com.example.hittable.Hittable
import com.example.vec3.Vec3
import cats.syntax.foldable
import cats.syntax.all.toTraverseOps
import com.example.Common.randomDouble
import com.example.vec3.Vec3.randomOnHemisphere
import com.example.hittable.HitRecord
import scala.annotation.tailrec
import com.example.vec3.Vec3.randomUnitVector

case class Camera(
    imageRatio: Double,
    imageWidth: Int,
    samplesPerPixel: Int,
    maxDepth: Int,
    imageHeight: Int,
    center: Vec3,
    pixel100Loc: Vec3,
    pixelDeltaU: Vec3,
    pixelDeltaV: Vec3
)
object Camera:

  def apply(
      imageRatio: Double,
      imageWidth: Int,
      samplePerPixel: Int = 10,
      maxDepth: Int = 10
  ): Camera =
    val imageHeight = (imageWidth / imageRatio).toInt
    val center = Vec3(0, 0, 0)

    val focalLength = 1.0
    val viewportHeight = 2.0
    val viewportWidth = (imageWidth.toDouble / imageHeight) * viewportHeight

    val viewportU = Vec3(viewportWidth, 0, 0)
    val viewportV = Vec3(0, -viewportHeight, 0)

    val pixelDeltaU = viewportU / imageWidth
    val pixelDeltaV = viewportV / imageHeight
    val viewportUpperLeft =
      center - Vec3(0, 0, focalLength) - (viewportU / 2.0) - (viewportV / 2.0)

    val pixel100Loc = viewportUpperLeft + 0.5 *: (pixelDeltaU + pixelDeltaV)
    Camera(
      imageRatio,
      imageWidth,
      samplePerPixel,
      maxDepth,
      imageHeight,
      center,
      pixel100Loc,
      pixelDeltaU,
      pixelDeltaV
    )

  extension (self: Camera)
    def render(world: HittableObject): IO[Unit] =
      header *> renderImage(world) *> IO {
        Console.err.println("\n\rDone.")
      }

    def header: IO[Unit] = IO {
      println(s"P3\n ${self.imageWidth} ${self.imageHeight}\n255\n")
    }

    def renderImage(world: HittableObject): IO[Unit] =
      for {
        _ <- (0 until self.imageHeight).toList.traverse(j =>
          renderCol(j)(world)
        )
      } yield ()

    def renderCol(j: Int)(world: HittableObject): IO[Unit] =
      for {
        _ <- printProcess(j)
        _ <- (0 until self.imageWidth).toList.traverse(i =>
          renderPixel(i, j)(world)
        )
      } yield ()

    def renderPixel(i: Int, j: Int)(world: HittableObject): IO[Unit] =
      val samplePixel = (0 until self.samplesPerPixel).toList
        .map(_ => {
          val r = getRay(i, j)
          val pixelColor = rayColor(r, world, 1.0, self.maxDepth)
          pixelColor
        })
        .foldLeft(Color(0, 0, 0))(_ + _)

      samplePixel.writeColor(self.samplesPerPixel)

    def printProcess(j: Int): IO[Unit] =
      IO {
        Console.err.print(s"\rScanlines reminaing: ${self.imageHeight - j}")
        Console.err.flush()
      }

    def pixelSampleSquare =
      // Returns a random point in the square surrounding a pixel at the origin.
      val px = -0.5 + randomDouble()
      val py = -0.5 + randomDouble()
      px *: self.pixelDeltaU + py *: self.pixelDeltaV

    private def getRay(i: Int, j: Int): Ray =
      // Get a randomly sampled camera ray for the pixel at location i,j.
      val pixelCenter =
        self.pixel100Loc + (i *: self.pixelDeltaU) + (j *: self.pixelDeltaV)
      val pixelSample = pixelCenter + pixelSampleSquare
      val rayOrigin = self.center
      val rayDirection = pixelSample - rayOrigin
      Ray(rayOrigin, rayDirection)

  @tailrec
  private def rayColor(
      r: Ray,
      world: HittableObject,
      factor: Double,
      depth: Int,
      renderedColor: Color = Color(1, 1, 1)
  )(using
      hittable: Hittable[HittableObject]
  ): Color =
    import com.example.Material.* 
    import com.example.Material.given
    if depth <= 0 then Color(0, 0, 0)
    else
      world.hit(r, Interval(0.001, Double.PositiveInfinity)) match
        case Some((rec, material:Material)) => {
          val scatter: Option[(Ray, Color)] = material.scatter(r, rec)
          scatter match
            case Some((scattered, attenuation)) =>
              rayColor(scattered, world, 0.5 * factor, depth - 1, attenuation)
            case None => Color(0, 0, 0)
          // val direction = rec.normal + randomUnitVector()
          // rayColor(Ray(rec.p, direction), world, 0.5 * factor, depth - 1)
        }
        case None =>
          val unitDirection = r.direction.unitVector
          val a = 0.5 * (unitDirection.y + 1.0)
          val color =
            (1.0 - a) *: Color(1.0, 1.0, 1.0) + a *: Color(0.5, 0.7, 1.0)
          color * factor * renderedColor

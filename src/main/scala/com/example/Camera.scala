package com.example

import com.example.hittable.HittableObject
import cats.effect.IO
import com.example.hittable.Hittable
import com.example.vec3.Vec3
import cats.syntax.foldable
import cats.syntax.all.toTraverseOps

case class Camera(
    imageRatio: Double,
    imageWidth: Int,
    imageHeight: Int,
    center: Vec3,
    pixel100Loc: Vec3,
    pixelDeltaU: Vec3,
    pixelDeltaV: Vec3
)
object Camera:

  def apply(imageRatio: Double, imageWidth: Int): Camera =
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
    Camera(imageRatio, imageWidth, imageHeight, center, pixel100Loc, pixelDeltaU, pixelDeltaV)

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
        _ <- (0 until self.imageHeight).toList.traverse(j => renderCol(j)(world))
      } yield ()

    def renderCol(j: Int)(world: HittableObject): IO[Unit] =
      for {
        _ <- printProcess(j)
        _ <- (0 until self.imageWidth).toList.traverse(i => renderPixel(i, j)(world))
      } yield ()

    def renderPixel(i: Int, j: Int)(world: HittableObject): IO[Unit] =
      val pixelCenter =
        self.pixel100Loc + (i *: self.pixelDeltaU) + (j *: self.pixelDeltaV)
      val rayDirection = pixelCenter - self.center
      val ray = Ray(self.center, rayDirection)

      val pixelColor = rayColor(ray, world)
      pixelColor.writeColor

    def printProcess(j: Int): IO[Unit] =
      IO {
        Console.err.print(s"\rScanlines reminaing: ${self.imageHeight - j}")
        Console.err.flush()
      }

  private def rayColor(r: Ray, world: HittableObject)(using
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

package com.example

import cats.effect.IOApp
import cats.effect.IO
import cats.effect.syntax.all
import cats.syntax.all.toTraverseOps
import cats.instances.ListInstances

object Render extends IOApp.Simple:
  val imageWidth = 256
  val imageHeight = 256


  def renderPixel(i: Int, j: Int): IO[Unit] =
    val color = Color(j / imageWidth.toDouble, i / imageHeight.toDouble, 100)
    color.writeColor

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

  def formatPixel(pixel: (Int, Int, Int)): String =
    s"${pixel._1} ${pixel._2} ${pixel._3}"

  def header: IO[Unit] = IO {
    println(s"P3\n $imageWidth $imageHeight\n255\n")
  }

  def run: IO[Unit] =
    header *> renderImage *> IO {
      Console.err.println("\n\rDone.")
    }

package com.example

object Common:
  val pi = 3.1415926535897932385

  inline def degreesToRadians(degrees: Double): Double =
    degrees * pi / 180.0


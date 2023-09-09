package com.example

object Common:
  val pi = 3.1415926535897932385

  inline def degreesToRadians(degrees: Double): Double =
    degrees * pi / 180.0

    // may be it can be updated to using the state monad?
  inline def randomDouble(): Double =
    scala.util.Random.between(0.0, 1.0)

  inline def randomDouble(min: Double, max: Double) =
    scala.util.Random.between(min, max)

package com.example

case class Interval(min: Double, max: Double)
object Interval:
  def apply(): Interval =
    Interval(Double.PositiveInfinity, Double.NegativeInfinity)

  val empty = Interval()
  val universe = Interval(Double.NegativeInfinity, Double.PositiveInfinity)

  
  extension (self: Interval)
    def contains(x: Double): Boolean = x >= self.min && x <= self.max

    def surrounds(x: Double): Boolean =
      x > self.min && x < self.max

    def clamp(x: Double): Double = 
      if x < self.min then self.min
      else if x > self.max then self.max
      else x

package scalaTest1

import scala.Math._

object Test1 {
	def main(args: Array[String]) :Unit = {
	  print(mySqrt(100))
	}
	
	def mySqrt(x: Double): Double = {
	  def sqrtIter(rt: Double, x:Double): Double = {
	    if(isGoodEnough(rt, x)) rt
	    else sqrtIter(improve(rt, x),x)
	  }
	  
	  def isGoodEnough(rt: Double, x:Double): Boolean = {
	    abs(rt*rt - x) < 0.0001
	  }
	  
	  def improve(rt:Double, x:Double):Double = {
	    (rt+x/rt)/2
	  }
	  
	  sqrtIter(1.0, x)
	}
}
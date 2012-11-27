package scalaTest1

import scala.Math._

object Test1 {
	def main(args: Array[String]) :Unit = {
	  print(mySqrt(22))
	}
	
	def mySqrt(x: Double): Double = {
	  def sqrtIter(rt: Double): Double = {
	    if(isGoodEnough(rt)) rt
	    else sqrtIter(improve(rt))
	  }
	  
	  def isGoodEnough(rt: Double): Boolean = {
	    abs(rt*rt - x) < 0.0001
	  }
	  
	  def improve(rt:Double):Double = {
	    (rt+x/rt)/2
	  }
	  
	  sqrtIter(1.0)
	}
}
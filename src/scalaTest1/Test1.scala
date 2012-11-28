package scalaTest1

import scala.Math._

object Test1 {
	def main(args: Array[String]) :Unit = {
	  println(mySqrt(22))
	  
	  val ans = queens(8)
	  for(i <- ans)
	    println(i)
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
	
  def queens(n: Int): List[List[(Int, Int)]] = {
    def placeQueens(k: Int): List[List[(Int, Int)]] =
      if (k == 0) 
        List(List())
      else 
        for {
          queens <- placeQueens(k - 1)
          column <- 1 to n
          queen = (k, column)
          if isSafe(queen, queens) 
        } yield queen :: queens

      def isSafe(queen: (Int, Int), queens: List[(Int, Int)]) = 
      	queens forall (q => !inCheck(queen, q))

	  def inCheck(q1: (Int, Int), q2: (Int, Int)) = 
	    q1._1 == q2._1 ||  // same row
	    q1._2 == q2._2 ||  // same column
	    (q1._1 - q2._1).abs == (q1._2 - q2._2).abs // on diagonal
        
    placeQueens(n)
  }	
}
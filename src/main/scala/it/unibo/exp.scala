package it.unibo

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import Builtins._

import scala.collection.mutable.{Map => MMap}

class exp extends AggregateProgram with ScafiAlchemistSupport with StandardSensors {
  lazy val source = mid()==0
  lazy val alert = (for(id <- Seq(40,50);
                       k  <- Seq(3,4,5)) yield (id+k)).contains(mid)
  lazy val batchFreq = env.get[Int]("batchFreq").toLong
                       
  def measureExecutionTime[T](expr: => T): T = {
    val startTime = System.nanoTime()
    val res = expr
    env.put[Double]("roundExecTime", meanCounter(System.nanoTime()-startTime, batchFreq))
    res
  }
  
   /*****************************
   ** MAIN PROGRAM ENTRY POINT **
   ******************************/
  override def main() = measureExecutionTime[Set[ID]] {
    env.put("source", source)
    env.put("alert", alert)
    
    val d = distanceTo(source)
    env.put[ID]("parent", findParent(d))
    //average(source, mid())
    summarise[Set[ID]](source, _++_, if(alert) Set(mid()) else Set(), Set())
  }
  
  /*************
   ** BLOCK C **
   *************/
  def smaller[V: Bounded](a: V, b: V):Boolean =
    implicitly[Bounded[V]].compare(a,b)<0

  def findParent[V: Bounded](potential: V): ID = {
    mux(smaller(minHood { nbr(potential) }, potential)) {
      minHood { nbr { (potential, mid()) } }._2
    } {
      Int.MaxValue
    }
  }

  def C[P: Bounded, V](potential: P, acc: (V, V) => V, local: V, Null: V): V = {
    rep(local) { v =>
      acc(local, foldhood(Null)(acc) {
        mux(nbr(findParent(potential)) == mid()) {
          nbr(v)
        }{
          nbr(Null)
        }
      })
    }
  }

  def average(sink: Boolean, value: Double): Double =
    summarise[Double](sink, _+_, value, 0.0) / summarise[Double](sink, _+_, 1, 0.0)
    
  def summarise[V](sink: Boolean, acc:(V,V)=>V, local:V, Null:V): V =
    C[Double,V](distanceTo(sink), acc, local, Null)
  
  /*************
   ** BLOCK G **
   *************/  
    
 def minHoodPLoc[A](default: A)(expr: => A)(implicit poglb: Ordering[A]): A = {
    import scala.math.Ordered.orderingToOrdered
    foldhoodPlus[A](default)((x, y) => if(x <= y) x else y){expr}
  }

  implicit def tupleOrd[A:Ordering, B:Ordering, C]: Ordering[(A,B,C)] = new Ordering[(A,B,C)] {
    import scala.math.Ordered.orderingToOrdered
    override def compare(x: (A, B, C), y: (A, B, C)): Int = (x._1,x._2).compareTo((y._1,y._2))
  }

  def G[V](source: Boolean, field: V, acc: V=>V, metric: => Double)
          (implicit idOrd: Ordering[ID]): V = {
    rep(Double.PositiveInfinity, mid, field) { case (dist, _, value) =>
      mux(source) {
        (0.0, mid, field)
      } {
        minHoodPLoc((Double.PositiveInfinity, mid, field)){ (nbr{ dist } + metric, nbr{ mid }, acc(nbr{ value })) }
      }
    }._3
  }    
  
//  def G[V: Bounded](source: Boolean, field: V, acc: V => V, metric: => Double): V =
//    rep((Double.MaxValue, field)) { case (dist, value) =>
//      mux(source) { (0.0, field)  } {
//        minHoodPlus { (nbr{dist} + metric, acc(nbr{ value })) }
//      }
//    }._2
//
  def G2[V](source: Boolean)(field: V)(acc: V => V)(metric: => Double = nbrRange): V =
    G(source, field, acc, metric)

  def distanceTo(source: Boolean): Double =
    G2(source)(0.0)(_ + nbrRange)()
    
  def broadcast[V: Bounded](source: Boolean, field: V): V =
    G2(source)(field)(v=>v)()
    
  /**********
   ** MISC **
   **********/      
    
  def meanCounter(value: Double, frequency: Long): Double = {
	  val time = currTime
	  val delta = dt()
	  val count = rep ((0.0,0.0)) { case x => { // (accumulated value, last time)
	    // Splits into windows of multiples of 'frequency'
	    // and restarts at the beginning of each new window.
	    // E.g., for frequency=5
	    // Time:           0_____5_____10_____15_____20 ...
	    // Restart:              ^      ^      ^      ^ ...
	    // Floor(Time/freq):  0     1      2      3     ... 
		  val restart = rep((false, time)) { t =>
			  (Math.floor(time/frequency) > Math.floor(t._2/frequency), time)
		  }._1
		  // Reset value and time on restart
		  val old = if (restart) (0.0,0.0)  else x
		  // Filters infinite values out
		  if (Double.NegativeInfinity < value && value < Double.PositiveInfinity) {
		    // Sums value weighed by time
		    (old._1+value*delta, old._2+delta)
		  } else old
	  } }
	  // E.g., consider these values and deltas: (5.0,2), (6,1), (Inf,2), (7,1), (5,1)
		// You'll finally have (5.0*2 + 6*1 + 7*1 + 5*1) / (2+1+1+1) = 28/5 = 5.6
	  count._1 / count._2
  }
}
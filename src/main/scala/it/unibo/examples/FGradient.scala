package it.unibo.examples

import it.unibo.ScafiAlchemistSupport
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._

class FGradient extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with ExplicitFields {

  override type MainResult = Any
  override def main = {
    val g1 = gradientField(mid==0)
    val g2 = classicGradient(mid==0)
    val g3 = distTo(mid==0)
    val g4 = G[Double](mid==0, 0.0, (_:Double)+nbrRange(), nbrRange())
    env.put("g1", g1)
    env.put("g2", g2)
    env.put("g3", g3)
    env.put("g4", g4)
    (g1,g2,g3,g4)
  }

  def gradientField(src: Boolean) = rep(Double.PositiveInfinity)(distance =>
    mux(src){ 0.0 }{
      (fnbr(distance)+fsns(nbrRange)).minHoodPlus
    }
  )

  def classicGradient(source: Boolean, metric: => Double = nbrRange()) = rep(Double.PositiveInfinity){ case d =>
    mux(source){ 0.0 }{ minHoodPlus(nbr(d)+metric) }
  }

  /**
    * BUG FOUND: I cannot minimise only on the gradient of my neighbour;
    * I need to sum the metric to the gradient of my neighbour;
    * but this means I cannot totally abstract from the metric by encapsulating it into the gradient
    */
  def G[V](gradient: () => Double, field: V, acc: V => V): V = {
    val g = gradient()
    rep(field) { case (value) =>
      //mux(g==0.0){ field }{ foldhood[(Double,V)]((Double.PositiveInfinity, field))( (x,y) => if(x._1 <= y._1) x else y ){
      mux(g==0.0){ field }{
        val k = rep(0)(_+1)
        var map = Map[ID,(ID,Double,V,V,Double)]()
        val minimized = includingSelf.minHoodSelector(nbr{g}+nbrRange()){
          val all = (nbr{mid},nbr{g},nbr{value},acc(nbr{value}),nbrRange())
          env.put("zzz_"+mid+"_aaa_"+nbr{mid}, all)
          map = map + (all._1 -> all)
          all
        }.getOrElse((-1,g,value,field,Double.PositiveInfinity))
        val all = map.getOrElse(minimized._1, (-1,0,field,field))
        if(k<10)        env.put("zzz_"+mid+"_minimized_"+k, all)
        minimized._4
      }
    }
  }

  def G[V: Builtins.Bounded](source: Boolean, field: V, acc: V => V, metric: => Double): V =
    rep((Double.MaxValue, field)) { case (dist, value) =>
      mux(source) {
        (0.0, field)
      } {

        minHoodPlus {
          val nbrg = nbr { dist } + metric
          val nbrv = nbr { value }
          val accv = acc(nbrv)
          val nbrmid = nbr{mid}
          val m = metric
          val k = rep(0)(_+1)
          if(k<10) {
            env.put("nbr_" + k + "_" + nbrmid + "_1gg", nbrg + " + " + m)
            env.put("nbr_" + k + "_" + nbrmid + "_2vg", nbrv)
            env.put("nbr_" + k + "_" + nbrmid + "_3accvg", accv)
          }
          (nbrg, accv)
        }
      }
    }._2

  def broadcast[V](source: Boolean, field: V): V =
    G[V](() => distTo(source), field, (v: V) => v)

  def distTo(source: Boolean, metric: => Double = nbrRange()): Double =
    G[Double](() => classicGradient(source, metric), 0.0, (d: Double) => d+metric)
}
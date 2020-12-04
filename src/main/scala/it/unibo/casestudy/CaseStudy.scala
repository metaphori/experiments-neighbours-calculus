package it.unibo.casestudy

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.ScafiAlchemistSupport

class CaseStudy extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with FieldUtils with ExplicitFields {

  override type MainResult = Any
  override def main = {
    gradientField(mid==0)
  }

  def gradientField(src: Boolean) = rep(Double.PositiveInfinity)(distance =>
    mux(src){ 0.0 }{
      (fnbr(distance)+fsns(nbrRange)).minHoodPlus
    }
  )
}
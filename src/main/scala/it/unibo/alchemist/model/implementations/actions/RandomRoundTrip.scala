package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.implementations.environments.OSMEnvironment
import it.unibo.alchemist.model.implementations.movestrategies.speed.ConstantSpeed
import it.unibo.alchemist.model.implementations.routes.PolygonalChain
import it.unibo.alchemist.model.interfaces._
import it.unibo.alchemist.model.interfaces.movestrategies.TargetSelectionStrategy
import it.unibo.alchemist.scala.PimpMyAlchemist._
import org.apache.commons.math3.random.RandomGenerator
import org.apache.commons.math3.util.FastMath.{atan2, cos, sin}

private object RandomPosition {
  private def randomInRange(rng: RandomGenerator, min: Double, max: Double) = rng.nextDouble() * (max - min) + min
  def apply(env: Environment[_], rng: RandomGenerator, minX: Double, minY: Double, maxX: Double, maxY: Double): Position =
    env.makePosition(randomInRange(rng, minX, maxX), randomInRange(rng, minY, maxY))
}

class RandomRoundTrip[T](environment: Environment[T], node: Node[T],
                         protected val reaction: Reaction[T],
                         rng: RandomGenerator,
                         val base: Position,
                         minX: Double, minY: Double,
                         maxX: Double, maxY: Double,
                         waypointCount: Int,
                         speed: Double) extends AbstractConfigurableMoveNode(
    environment, node,
    (start, end) => base match {
      case `end` => new PolygonalChain(base)
      case `start` =>
        val waypoints = (2 to waypointCount).map(_ => RandomPosition(environment, rng, minX, minY, maxX, maxY)).toList
        new PolygonalChain[Position](start :: waypoints ::: end :: Nil :_*)
      case _ => throw new IllegalStateException(s"Either the start ($start) or the end ($end) must match with the base ($base)")
    },
    new TargetSelectionStrategy {
      var currentTarget: Position = base
      override def getTarget: Position = {
        currentTarget = environment.getPosition(node) match {
          case `base` => RandomPosition(environment, rng, minX, minY, maxX, maxY)
          case p if p == currentTarget => base
          case _ => currentTarget
        }
        currentTarget
      }
    },
    new ConstantSpeed(reaction, speed),
  true
  ) {

  def this(environment: Environment[T], node: Node[T], reaction: Reaction[T], rng: RandomGenerator,
    baseX: Double, baseY: Double,
    minX: Double, minY: Double,
    maxX: Double, maxY: Double,
    waypointCount: Int,
    speed: Double) = this(environment, node, reaction, rng, environment.makePosition(baseX, baseY), minX, minY, maxX, maxY, waypointCount, speed)

  override def getDestination(source: Position, target: Position, maxWalk: Double): Position = {
    val desiredDistance = source.getDistanceTo(target)
    if (desiredDistance <= maxWalk) target
    else {
      val ratio = maxWalk / desiredDistance
      val vector = (target - source)
      val res: Position = if (source.isInstanceOf[GeoPosition]) {
        environment.makePosition(vector.getCoordinate(1) * ratio, vector.getCoordinate(0) * ratio)
      } else {
        environment.makePosition(vector.getCoordinate(0) * ratio, vector.getCoordinate(1) * ratio)
      }
      val actualdest = source + res
      actualdest
    }
  }

  override def cloneAction(node: Node[T], reaction: Reaction[T]): Action[T] =
    ???

}

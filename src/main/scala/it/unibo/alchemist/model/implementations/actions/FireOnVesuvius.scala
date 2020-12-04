package it.unibo.alchemist.model.implementations.actions

import it.unibo.alchemist.model.implementations.molecules.SimpleMolecule
import it.unibo.alchemist.model.interfaces._
import org.apache.commons.math3.random.RandomGenerator

class FireOnVesuvius(
      protected val environment: Environment[Any],
      node: Node[Any],
      reaction: Reaction[Any],
      protected val randomGenerator: RandomGenerator,
      sensor: Molecule
) extends AbstractActionOnSingleMolecule[Any](node, sensor) {

  import FireOnVesuvius._
  import scala.collection.JavaConverters._

  sensorValues.keys.foreach(mol => (environment.asScala ++ Seq(node))
    .foreach(_.setConcentration(mol,
    (environment.makePosition(randomGenerator.nextDouble() * 0.072156 + 40.784553, randomGenerator.nextDouble() * 0.06 + 14.4), sensorValues.get(mol)),
  )))

  override def cloneAction(node: Node[Any], reaction: Reaction[Any]): Action[Any] = ???

  override def execute(): Unit = {
    val time = reaction.getTau.toDouble
    val localMax: Double = sensorValues.keys
      .map(node.getConcentration(_))
      .map(_.asInstanceOf[(Position, Some[(Double, Double, Double)])])
      .map(t => (t._1.getDistanceTo(environment.getPosition(node)), time match {
        case _ if time < timeInterval => t._2.value._1
        case _ if time > 2 * timeInterval => t._2.value._3
        case _ => t._2.value._2
      }))
      .map { case (d, v) => v / ( 1 + Math.pow(d / 100, 2))}
      .max
    require(!localMax.isNaN)
    node.setConcentration(sensor, localMax)
  }


  override def getContext: Context = Context.LOCAL
}

object FireOnVesuvius {
  implicit def string2Mol(s: String): Molecule = new SimpleMolecule(s)
  implicit def int2Mol(i: Int): Molecule = i.toString
  val timeInterval = 1200
  def truth(t: Double): Double = ((t / timeInterval) match {
    case interval if interval <= 1 => sensorValues.get(1)
    case interval if interval <= 2 => sensorValues.get(2)
    case _ => sensorValues.get(3)
  }).get.productIterator.map(_.asInstanceOf[Double]).max

  val sensorValues: Map[Int, (Double, Double, Double)] = Map(
    1 -> (1.0, 0.2, 0.2),
    2 -> (0.1, 0.2, 0.3),
    3 -> (0.3, 0.3, 0.9),
  )
}
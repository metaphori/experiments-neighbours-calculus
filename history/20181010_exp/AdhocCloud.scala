package it.unibo.casestudy

import it.unibo.ScafiAlchemistSupport
import it.unibo.alchemist.model.implementations.positions.LatLongPosition
import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.scafi.core.Core
import it.unibo.scafi.space.Point3D

trait DiskType
case object HDD extends DiskType
case object SSD extends DiskType

case class Percent(p: Double) {
  //assert(p>=0 && p<=100)

  def of(value: Double): Double = value/100.0*p
}
object Percent {
  implicit def toDouble(p: Percent): Double = p.p
  def of(v1: Double, v2: Double): Percent = Percent(v2*100/v1)
}

case class ProcessingStat(numCPUs: Int = 0, mHz: Int = 0, numCPUsInUse: Int = 0) {
  val inUse = Percent.of(numCPUs,numCPUsInUse)
}
case class MemoryStat(MiBs: Int = 0, MiBsInUse: Int = 0){
  val inUse = Percent.of(MiBs, MiBsInUse)
}
case class StorageStat(MiBs: Int = 0, dtype: DiskType = HDD, MiBsInUse: Int = 0) {
  val inUse = Percent.of(MiBs, MiBsInUse)
}
case class LocalStats(processing: ProcessingStat = ProcessingStat(),
                      memory: MemoryStat = MemoryStat(),
                      storage: StorageStat = StorageStat()){
  val inUse = Percent((processing.inUse.p + memory.inUse.p + storage.inUse.p)/3)
}
case class AggregateStats(cpus: Int = 0, memoryMiBs: Int = 0, storageMiBs: Int = 0,
                          cpusInUse: Long = 0, memoryMiBsInUse: Long = 0, storageMiBsInUse: Long = 0)
case class AreaStats(leader: Int)(val stats: AggregateStats){
  override def toString: String = s"($leader, $stats)"
}
case class SpatialData(leader: Int, leaderPosition: LatLongPosition, meanDensity: Double)

// The function requires an Aggregate Program to avoid issues related to passing functions
// belonging to different AggregatePrograms.
case class Fun[R](ver: Int, fun: (AggregateProgram with StandardSensors)=>R)

case class Gradient(algorithm: (Boolean,()=>Double)=>Double, source: Boolean, metric:()=>Double){
  def run(): Double = algorithm(source, metric)
}

case class RequestMoreResources(leader: Int)(val position: LatLongPosition)

class AdhocCloud extends AggregateProgram
  with StandardSensors with ScafiAlchemistSupport with BlockG with BlockC with FieldUtils { self =>

  val RequestHandled = "request_handled"
  val IsMoving = "is_moving"
  val MoveTo = "move_to"

  override type MainResult = Any
  type Stats = Map[ID,LocalStats]

  lazy val idle =
    if(getPosition().y>44.1379 && nextRandom>0.38 ) true else if(nextRandom>0.99) true else false
  lazy val exhausted =
    mid%3!=0

  lazy val changeEveryMaxRounds =
    env.get[Int]("changeEveryMaxRounds")

  def localProcessors(initRandom: Double, dynamicRandom: Double) = {
    val ncpus = if(initRandom>0.8) 8 else if(initRandom>0.6) 4 else if(initRandom > 0.4) 2 else 1
    ProcessingStat(
      numCPUs = ncpus,
      mHz = (1 + Math.round(initRandom * 1).toInt)*1000,
      numCPUsInUse = /*if(idle) 0 else if(exhausted) ncpus else*/ (dynamicRandom * ncpus).toInt)
    // sense[ProcessingStat]("statsCPU")
  }
  def localMemory (initRandom: Double, dynamicRandom: Double) = {
    val mibs = 1 + (initRandom * 10).toInt*512
    MemoryStat(
      MiBs = mibs,
      MiBsInUse = if(idle) 0 else if(exhausted) (mibs/100.0*90).toInt else (dynamicRandom * mibs).toInt)
    // sense[MemoryStat]("statsRAM")
  }
  def localStorage(initRandom: Double, dynamicRandom: Double) = {
    val mibs = (1 + Math.round(initRandom * 50).toInt*2)*512
    StorageStat(MiBs = mibs,
      dtype = if(dynamicRandom>0.8) SSD else HDD,
      MiBsInUse = if(idle) 0 else (dynamicRandom * mibs).toInt)
    // sense[StorageStat]("statsStorage")
  }

  def metricInjecter(k: Int): Injecter[Double] = () =>
    if(mid != 0 || k < 250){ Fun(0, ap => ap.nbrRange()) } else { Fun(1, ap => ap.nbrRange()+1.0) }

  lazy val lowDensity: Double = env.get[Double]("lowDensityThreshold")
  lazy val highDensity: Double = env.get[Double]("highDensityThreshold")
  lazy val lowLoad: Double = env.get[Double]("lowLoad")
  lazy val highLoad: Double = env.get[Double]("highLoad")

  def getPosition(): LatLongPosition =
    sense[LatLongPosition]("position")

  implicit def fromLatLogToPoint(ll: LatLongPosition): Point3D =
    Point3D(ll.getLongitude, ll.getLatitude, 0)

  lazy val initialization: Unit = {
    env.put(RequestHandled, Option.empty[RequestMoreResources])
    env.put(IsMoving, false)
    env.put("distance", 0.0)
    env.put(MoveTo, new LatLongPosition(0,0))
  }

  def keepTrackOfDistance() =     rep(getPosition()){ case pos =>
    env.put("distance", env.get[Double]("distance") + getPosition().getDistanceTo(pos))
    getPosition()
  }

  override def main(): Any = {
    initialization
    val k = rep(0)(_+1)
    val initRandom = rep(nextRandom){ r => r }
    val dynamicRandom = rep((20+(nextRandom*changeEveryMaxRounds).toInt, nextRandom)) { case (step,rand) =>
      //env.put("dynamic_random", s"$k => ${k%step} => Every $step changes to $rand")
      if(k % step == 0) (step,nextRandom + (if(k>300 && k<500) 0.1 else 0)) else (step,rand)}._2

    val localResources = LocalStats(localProcessors(initRandom, dynamicRandom), localMemory(initRandom, dynamicRandom), localStorage(initRandom, dynamicRandom))

    env.put("this",self.hashCode())
    env.put("this_position", getPosition())
    env.put("local_resources", localResources)
    env.put("available_cpu", localResources.processing.numCPUs)
    env.put("usage_cpu", localResources.processing.numCPUsInUse)
    env.put("usage_memory", localResources.memory.MiBsInUse)
    env.put("usage_storage", localResources.storage.MiBsInUse)
    env.put("usage", (localResources.processing.inUse.p + localResources.memory.inUse.p +localResources.storage.inUse.p)/3.0)
    // keepTrackOfDistance()

    val metricfun: Fun[Double] = up[Double](metricInjecter(k))
    env.put("metric", metricfun)
    val metric: () => Double = () => metricfun.fun(this) // nbrRange // NB: trick to avoid issues related to shipping lambdas from other classes

    val leaders = S(env.get[Double]("grain"), metric)
    val potential = distTo(leaders) // metric

    val resourcesPerArea = C[Double,Stats](potential, _++_, Map(mid -> localResources), Map())
    val aggregatedResourcesPerArea = resourcesPerArea.values.foldLeft(AggregateStats())((a1, a2) =>
      AggregateStats(
        a1.cpus + a2.processing.numCPUs,
        a1.memoryMiBs + a2.memory.MiBs,
        a1.storageMiBs + a2.storage.MiBs,
        cpusInUse = a1.cpusInUse + a2.processing.numCPUsInUse,
        memoryMiBsInUse = a1.memoryMiBsInUse + a2.memory.MiBsInUse,
        storageMiBsInUse = a1.storageMiBsInUse + a2.storage.MiBsInUse,
      ))
    val numNodesInArea = C[Double,Int](potential, _+_, 1, 0)

    val allResourcesInArea = broadcast(leaders, aggregatedResourcesPerArea)

    val numNbrs = excludingSelf.sumHood(1)
    val numNbrsInArea = C[Double,Double](potential, _+_, numNbrs, 0)
    val spatialDataForArea = broadcast(leaders, SpatialData(mid, getPosition(), numNbrsInArea/numNodesInArea))
    val meanDensity = spatialDataForArea.meanDensity

    val myLeader = broadcast(leaders, mid)
    val nbrAreas = excludingSelf.unionHood(nbr{AreaStats(myLeader)(allResourcesInArea)})

    env.put("estimated_usage_cpu", if(leaders) aggregatedResourcesPerArea.cpusInUse else 0)

    val adjacentAreasStats: Set[AreaStats] = broadcast(leaders, C[Double,Set[AreaStats]](potential, _.union(_), nbrAreas, Set()))

    val moreDevicesAllowed =
      meanDensity < highDensity
    val moreCPUNeeded =
      Percent.of(allResourcesInArea.cpus, allResourcesInArea.cpusInUse) > highLoad
    val mayMove = !leaders &&
      localResources.inUse.p == 0 &&
      meanDensity > lowDensity &&
      Percent.of(allResourcesInArea.cpus, allResourcesInArea.cpusInUse) < lowLoad &&
      sufficientlyStable(myLeader, 100)

    env.put("working_on_request", requestBeingHandled)
    branch(requestBeingHandled.isDefined){
      if(myLeader == requestBeingHandled.get.leader) doFinishRequest
    }{
      val requests = excludingSelf.unionHoodSet(nbr { mux(moreCPUNeeded){
        Set(RequestMoreResources(spatialDataForArea.leader)(spatialDataForArea.leaderPosition))
      }{ Set.empty } }).filter(_.leader!=myLeader)
      env.put("requests", requests)
      // Select one request
      if(mayMove && !requests.isEmpty) doHandleRequest(requests.head)
    }

    env.put("may_move", mayMove)
    env.put("may_move_str", s"$mayMove --- !busy (${localResources.inUse}) " +
      s" && !lowDensity ($meanDensity > $lowDensity = ${meanDensity > lowDensity}) && lowLoad (${Percent.of(allResourcesInArea.cpus, allResourcesInArea.cpusInUse)} < ${lowLoad})")
    env.put("more_devices_allowed", s"${meanDensity} < ${highDensity} = $moreDevicesAllowed")
    env.put("more_cpu_needed", s"${Percent.of(allResourcesInArea.cpus, allResourcesInArea.cpusInUse)} > ${highLoad} = $moreCPUNeeded")
    env.put("mean_density", meanDensity)
    env.put("leader", leaders)
    env.put("adjacent_area_stats", adjacentAreasStats)
    env.put("numNodes", if(leaders) numNodesInArea else 0)
    env.put("aggregateResourcesPerArea", aggregatedResourcesPerArea)
    env.put("allResourcesPerArea", allResourcesInArea)
    env.put("myLeader", myLeader)
    env.put("potential", potential)

    77
  }

  def doHandleRequest(req: RequestMoreResources) = { env.put(RequestHandled, Some(req)); env.put(IsMoving, true); env.put(MoveTo, req.position) }
  def doFinishRequest = { env.put(RequestHandled, None); env.put(IsMoving, false); env.put(MoveTo, getPosition()) }
  def isWorkingOnRequest = env.get(RequestHandled)!=None
  def requestBeingHandled: Option[RequestMoreResources] = if(isWorkingOnRequest) env.get(RequestHandled) else Option.empty

  def captureChange[T](x: T): Boolean = rep((x,false)) { case (value, _) =>
    (x, value != x)
  }._2

  def sufficientlyStable[T](x: T, k: Int) = rep((x,0,false)){ case (v,nrounds,suffStable) =>
      mux(v!=x){ (x, 0, false) }{ (x, nrounds+1, nrounds+1 >= k) }
  }._3

  def classicGradient(source: Boolean, metric: () => Double = nbrRange) = rep(Double.PositiveInfinity){ case d =>
    mux(source){ 0.0 }{ minHoodPlus(nbr(d)+metric()) }
  }

  def distTo(source: Boolean, metric: () => Double = nbrRange) =
    classicGradient(source, metric)

  def G[V](gradient: Gradient, field: V, acc: V => V): V = {
    val g = gradient.run()
    rep(field) { case (value) =>
      mux(g==0.0){ field }{ excludingSelf.minHoodSelector[Double,V](nbr{g}+gradient.metric())(acc(nbr{value})).getOrElse(field) }
    }
  }

  def broadcast[V](source: Boolean, field: V): V =
    G[V](Gradient(classicGradient,source, nbrRange), field, (v: V) => v)

  /**
    * NOTE: the transitory phase is different with respect to the "classical gradient".
    * I.e., devices are at a "distance" 0 until accumulation of values along the gradient make their distance raise.
    */
  def distToWithG(source: Boolean, metric: () => Double = nbrRange): Double =
    G[Double](Gradient(classicGradient, source, metric), 0.0, (d: Double) => d+metric())

  def S(grain: Double, metric: () => Double): Boolean =
    breakUsingUids(randomUid, grain, metric)

  def randomUid: (Double, ID) =
    rep((nextRandom), mid()) { v => (v._1, mid()) }

  def breakUsingUids(uid: (Double, ID),
                     grain: Double,
                     metric: () => Double): Boolean =
    uid == rep(uid) { lead: (Double, ID) =>
      distanceCompetition(distTo(uid == lead, metric), lead, uid, grain, metric)
    }

  /**
    * Candidate leader devices surrender leadership to the lowest nearby UID.
    *
    * @return
    */
  def distanceCompetition(d: Double,
                          lead: (Double, ID),
                          uid: (Double, ID),
                          grain: Double,
                          metric: () => Double): (Double, ID) = {
    val inf: (Double, ID) = (Double.PositiveInfinity, uid._2)
    mux(d > grain) {
      uid
    } {
      mux(d >= (0.5 * grain)) {
        inf
      } {
        minHood {
          mux(nbr { d } + metric() >= 0.5 * grain) {
            nbr { inf }
          } {
            nbr { lead }
          }
        }
      }
    }
  }

  // UPDATABLE METRIC
  type Injecter[R] = () => Fun[R]
  def up[R](injecter: Injecter[R]): Fun[R] = rep(injecter()){ case f =>
    val injfun = injecter()
    val nbrfun = includingSelf.maxHoodSelector(nbr{ f.ver })(nbr{ f }).getOrElse(injfun)
    if(injfun.ver > nbrfun.ver) injfun else nbrfun
  }

  implicit class RichFieldOps(fieldOps: FieldOps) {
    def maxHoodSelector[T: Builtins.Bounded, V](toMaximize: => T)(data: => V): Option[V] = {
      val ord = implicitly[Builtins.Bounded[T]]
      fieldOps.foldhoodTemplate[(T, Option[V])]((ord.bottom, None))((x, y) => if (ord.compare(x._1, y._1) > 0) x else y)((toMaximize, Some(data)))._2
    }
  }
}
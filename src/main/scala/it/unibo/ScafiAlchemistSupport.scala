package it.unibo

import it.unibo.alchemist.model.scafi.ScafiIncarnationForAlchemist._
import it.unibo.alchemist.implementation.nodes.NodeManager
import it.unibo.alchemist.model.interfaces.Time
import it.unibo.scafi.PlatformDependentConstants
import it.unibo.alchemist.model.interfaces.Environment

/*
 * Copyright (C) 2016-2017, Roberto Casadei, Mirko Viroli, and contributors.
 * See the LICENCE.txt file distributed with this work for additional
 * information regarding copyright ownership.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
*/

trait ScafiAlchemistSupport { self: AggregateProgram with StandardSensors =>
  def env = sense[NodeManager]("manager")
  def currTime: Double = sense[Time]("time").toDouble()
  def dt(whenNan: Double = Double.NaN): Double = {
    val dt = sense[Time]("dt").toDouble()
    if(dt.isNaN) whenNan else dt
  }
  def nextRandom: Double = sense[()=>java.lang.Double]("random")().toDouble
  def environment = sense[Environment[Any]]("env")

  import sun.reflect.Reflection.getCallerClass
  override def aggregate[T](f: => T): T =
    vm.nest(FunCall[T](vm.index, getCallerClass(PlatformDependentConstants.StackTracePosition).getName()))(true) {
      f
    }
}

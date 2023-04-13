package it.unibo.language

import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import it.unibo.util.{*, given}
import it.unibo.core.{Context, VM, Export}
import it.unibo.{Network, Device}
import it.unibo.Network._
class FieldCalculusTest extends AnyFlatSpec with Matchers {
  val emptyContext = Context(Map.empty, 0)

  def performOn[A](vm: VM)(computation: VM ?=> A): A =
    given VM = vm
    computation

  def simpleTopology(ids: Set[Int]): Network =
    val devices = ids.map(Device(_))
    val topology = ids.map(id => id -> ids).toMap
    Network(devices, topology)

  "rep" should "updated the local data in several rounds" in {
    var vm = VM(emptyContext)
    def program(using vm: VM): Int = FieldCalculus.rep(0)(_ + 1)
    performOn(vm)(program) shouldBe 1
    vm = VM(Context(vm.outExports, emptyContext.self))
    performOn(vm)(program) shouldBe 2
  }

  "nbr" should "create a field from other neighbors" in {
    val neighborhood = Set(1, 2, 3)
    def program(using vm: VM): Iterable[Int] = FieldCalculus.nbr(FieldCalculus.mid).values
    val network = simpleTopology(neighborhood)
    val result = neighborhood.map(id => id -> (network, network.select(id).get).run(program)).toMap
    // after the first computation, no one can see the other data
    val exports = result.view.mapValues(_._2).toMap
    val localData = result.values.map(_._1).forall(_.isEmpty) shouldBe true
    (network, exports).broadcast() // everyone receives the information needed by nbr
    val resultAfterCommunication =
      neighborhood.map(id => id -> (network, network.select(id).get).run(program)._1).toMap.values.map(_.toSet)
    resultAfterCommunication.forall(_ == Set(1, 2, 3)) shouldBe true
  }

  // TODO improve, create a support to simple simulation
  "branch" should "create two not communicating zone" in {
    def emptyContext(me: Int, other: Set[Int]): Context =
      Context(other.map(_ -> Export()).toMap, me)
    val neighborhood = Set(1, 2, 3)
    def program(using vm: VM): Set[Int] = {
      import FieldCalculus.*
      val id = mid
      branch(id > 2)(nbr(mid))(nbr(mid)).values.toSet
    }
    val network = simpleTopology(neighborhood)
    val result = neighborhood.map(id => id -> (network, network.select(id).get).run(program)).toMap
    // after the first computation, no one can see the other data
    val exports = result.view.mapValues(_._2).toMap
    val localData = result.values.map(_._1).forall(_.isEmpty) shouldBe true
    (network, exports).broadcast() // everyone receives the information needed by nbr
    val resultAfterCommunication =
      neighborhood.map(id => id -> (network, network.select(id).get).run(program)._1).toMap
    resultAfterCommunication(1) shouldBe Set(1, 2)
    resultAfterCommunication(2) shouldBe Set(1, 2)
    resultAfterCommunication(3) shouldBe Set(3)
  }
}

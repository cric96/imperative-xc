package it.unibo.language

import it.unibo.Network.*
import it.unibo.core.{Context, Export, VM}
import it.unibo.util.{*, given}
import it.unibo.{Device, Network}
import org.scalatest.*
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExchangeTest extends AnyFlatSpec with Matchers:
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
    def program(using vm: VM): Int = Exchange.rep(0)(_ + 1)
    performOn(vm)(program) shouldBe 1
    vm = VM(Context(vm.outExports, emptyContext.self))
    performOn(vm)(program) shouldBe 2
  }

  "nbr" should "create a field from other neighbors" in {
    val neighborhood = Set(1, 2, 3)
    def program(using vm: VM): Iterable[Int] = Exchange.nbr(FieldCalculus.mid).values
    val network = simpleTopology(neighborhood)
    (0 to 2) foreach { _ =>
      val result = neighborhood.map(id => id -> (network, network.select(id).get).run(program)).toMap
      // after the first computation, no one can see the other data
      val exports = result.view
        .mapValues(_._2)
        .toMap
      //val localData = result.values.map(_._1).forall(_.isEmpty) shouldBe true
      (network, exports).broadcast() // everyone receives the information needed by nbr
    }

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
      import Exchange.*
      val id = FieldCalculus.mid
      FieldCalculus.branch(id > 2)(nbr(FieldCalculus.mid))(nbr(FieldCalculus.mid)).values.toSet
    }
    val network = simpleTopology(neighborhood)
    (0 to 2) foreach { _ =>
      val result = neighborhood.map(id => id -> (network, network.select(id).get).run(program)).toMap
      // after the first computation, no one can see the other data
      val exports = result.view.mapValues(_._2).toMap
      (network, exports).broadcast() // everyone receives the information needed by nbr
    }
    val resultAfterCommunication =
      neighborhood.map(id => id -> (network, network.select(id).get).run(program)._1).toMap

    resultAfterCommunication(1) shouldBe Set(1, 2)
    resultAfterCommunication(2) shouldBe Set(1, 2)
    resultAfterCommunication(3) shouldBe Set(3)
  }

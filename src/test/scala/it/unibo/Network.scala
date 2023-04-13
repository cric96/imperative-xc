package it.unibo
import it.unibo.core.{Export, Context, VM}

trait Device:
  def id: Int
  def receive(id: Int, msg: Export): Unit
  def join(id: Int): Unit
  def leave(id: Int): Unit
  def messageQueue: Map[Int, Export]
  def createContext: Context

object Device:
  def apply(id: Int): Device = TestDeviceImpl(id, Map.empty)

  private class TestDeviceImpl(val id: Int, var messages: Map[Int, Export]) extends Device:
    def receive(id: Int, msg: Export): Unit = messages += (id -> msg)
    def join(id: Int): Unit =
      if (!messages.contains(id)) messages += (id -> Export())
    def leave(id: Int): Unit = messages -= id
    def messageQueue: Map[Int, Export] = messages
    def createContext: Context = Context(messageQueue, id)

trait Network:
  def select(id: Int): Option[Device]
  def neighborhood(id: Int): Set[Int]

object Network:
  def apply(devices: Set[Device], topology: Int => Set[Int]): Network = new Network:
    def select(id: Int): Option[Device] = devices.find(_.id == id)
    def neighborhood(id: Int): Set[Int] = topology(id)

  extension (world: (Network, Device))
    def run[A](program: VM ?=> A): (A, Map[Int, Export]) =
      val (network, device) = world
      network.neighborhood(device.id).foreach(device.join)
      given vm: VM = VM(device.createContext)
      val result = program
      //.foreach((id, msg) => network.select(id).foreach(_.receive(device.id, msg)))
      (result, vm.outExports)

  extension (world: (Network, Map[Int, Map[Int, Export]]))
    def broadcast(): Unit =
      val (network, messages) = world
      messages.foreach((from, messages) =>
        messages.foreach((to, message) => network.select(to).foreach(_.receive(from, message)))
      )

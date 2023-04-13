package it.unibo.core

import it.unibo.language.FieldCalculus

class VM(val context: Context):
  var status = VMStatus(Path())
  var outExports =
    Map.empty[Int, Export]

  def enter(tag: Tag): Unit =
    val index = status.index
    val slot = Slot(tag, index)
    status = status.push().nest(slot)

  def send(data: Map[Int, Local]): Unit =
    val toSend = data.map((id, local) => (id, local, outExports.getOrElse(id, Export())))
    toSend.foreach { case (_, local, exportData) => exportData.put(status.path, local) }
    outExports = outExports ++ toSend.map { case (id, _, exportData) => (id, exportData) }

  /**
   * Store a local value in the current path.
   * It can be used for expressing evolution of the local state.
   * @param local the data to store in the current path
   */
  def store(local: Local): Unit = send(Map(context.self -> local))

  def received: Map[Int, Local] =
    val path = status.path
    context.exports.collect {
      case (id, exportData) if exportData.paths.contains(path) => id -> exportData.get(path).get
    }

  def exit(): Unit =
    status = status.pop().incrementIndex()

class VMStatus(
    val path: Path = Path(), // current path in the evaluation tree
    val index: Int = 0, // current index in the evaluation tree
    stack: List[(Path, Int)] = List() // stack of calls
):
  // nest the current path with a new slot, i.e., add a new slot to the current path
  def nest(s: Slot): VMStatus = VMStatus(path.push(s), 0, stack)
  // increment the index after a nested call
  def incrementIndex(): VMStatus = VMStatus(path, index + 1, stack)
  // prepare for a nested call, i.e., push the current path and index in the stack
  def push(): VMStatus = VMStatus(path, index, (path, index) :: stack)
  // restore the path and index after a nested call, i.e., pop the current path and index from the stack
  def pop(): VMStatus = stack match
    case (p, i) :: s => VMStatus(p, i, s)
    case _ => throw new Exception()

class Context(val exports: Map[Int, Export], val self: Int):
  def neighbours: Set[Int] = exports.keySet

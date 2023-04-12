package it.unibo.core

import it.unibo.language.FieldCalculus

class VM(val context: Context):
  var status = VMStatus(Path())
  var exports = context.exports.map(_._1 -> Export())

  def enter(slot: Slot): Unit = ???
  def send(data: Map[Int, Local]) = ???
  def receive: Map[Int, Local] = ???
  def exit(): Unit = ???
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

class Context(val exports: Seq[(Int, Export)], val self: Int)

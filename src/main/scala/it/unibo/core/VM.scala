package it.unibo.core

import it.unibo.core.Slot.Exchange
import it.unibo.language.FieldCalculus

class VM(val context: Context):
  var status = VMStatus(Path())
  var exportData = Export()
  def exchangeEnter(): PrepareExchangeScope =
    val slot = Slot.Exchange(status.index)
    status = status.push()
    status = status.nest(slot)
    val neigh = context.exports.map { case (id, exp) => id -> exp.get(status.path) }
    PrepareExchangeScope(status.path, aligned = neigh.collect { case (id, Some(a: Any)) => id -> a }.toMap)

  def alignEnter(tag: Any): PrepareAlignScope =
    val slot = Slot.Align(tag, status.index)
    status = status.push()
    status = status.nest(slot)
    PrepareAlignScope(status.path)

  def exit(scope: ComputedScope): Any =
    status = status.pop().incrementIndex()
    scope match {
      case exchange: ExchangeScope =>
        exportData.put(scope.path, exchange.send)
      case align: AlignScope =>
        exportData.put(scope.path, align.data)
    }
    scope.data

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

sealed trait Scope:
  def path: Path

trait ComputedScope extends Scope:
  def data: Any
  def path: Path

case class PrepareAlignScope(path: Path) extends Scope:
  def completeWith(data: Any): AlignScope = AlignScope(path, data)

case class AlignScope(path: Path, data: Any) extends ComputedScope

case class PrepareExchangeScope(path: Path, aligned: Map[Int, Any]) extends Scope:
  def withDefaultValue(default: Any): ExchangeScopeDefaultSelected =
    ExchangeScopeDefaultSelected(path, default, aligned)

case class ExchangeScopeDefaultSelected(path: Path, default: Any, aligned: Map[Int, Any]) extends Scope:
  def completeWith(data: Map[Int, Any]): ExchangeScope = ExchangeScope(path, aligned, data, data, default = default)
  def completeWith(local: Map[Int, Any], send: Map[Int, Any]): ExchangeScope =
    ExchangeScope(path, aligned, local, send, default = default)

case class ExchangeScope(
    path: Path,
    alignedData: Map[Int, Any],
    data: Map[Int, Any],
    send: Map[Int, Any],
    default: Any
) extends ComputedScope {}

class Context(val exports: Seq[(Int, Export)], val self: Int)

object VM {
  def main(args: Array[String]): Unit = {
    given contextG: Context = new Context(Seq.empty, 2)

    val program: Program = new Program:
      override def run(): VM ?=> Any =
        FieldCalculus.rep(0)(_ + 1)

    val firstRun = program.logic()

    val newContext = new Context(Seq(2 -> Export.sentExportTo(2, firstRun)), 2)
    println(Export.sentExportTo(2, firstRun))
    val secondRun = program.logic()(using newContext)
    println(secondRun)
    val exports = Seq(
      2 -> new Export(Map(Path(List(Exchange(0))) -> 1, Path(List()) -> Map(2 -> 1))),
      3 -> new Export(Map(Path(List(Exchange(0))) -> 3, Path(List()) -> Map(3 -> 1)))
    )
    val nbrProgram = new Program:
      override def run(): VM ?=> Any =
        FieldCalculus.nbr(1).values.sum

    println(nbrProgram.logic()(using new Context(exports, 2)))
  }
}

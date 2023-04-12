package it.unibo.core

import it.unibo.core.Slot.Exchange

enum Slot:
  case Exchange(index: Int)
  case Align(tag: Any, index: Int)

case class Path(path: List[Slot] = List.empty):
  def push(slot: Slot): Path = Path(slot :: path)
  def pull(): Path = Path(path.tail)

class Export(private var map: Map[Path, Any] = Map.empty) extends Equals:
  def put(path: Path, value: Any): Any = { map += (path -> value); value }
  def get(path: Path): Option[Any] = map.get(path)
  def root(): Any = get(Path()).get
  def paths: Map[Path, Any] = map

  override def equals(o: Any): Boolean = o match {
    case x: Export => x.paths == map
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that match { case _: Export => true; case _ => false }

  override def hashCode(): Int = map.hashCode()

  override def toString: String = map.toString

object Export:
  def sentExportTo(id: Int, exportData: Export): Export =
    val newExport = Export()
    exportData.paths
      .filter {
        case (path @ Path((data: Exchange) :: rest), value: Map[Int, Any]) =>
          value.contains(id)
        case _ => true
      }
      .map {
        case (path @ Path((data: Exchange) :: rest), value: Map[Int, Any]) =>
          path -> value(id)
        case (path, value) => path -> value
      }
      .foreach { case (path, value) => newExport.put(path, value) }
    newExport

package it.unibo.core

trait Local // generic local data, internal representation for raw data to be sent

case class Slot(tag: Local, index: Int)

case class Path(path: List[Slot] = List.empty):
  def push(slot: Slot): Path = Path(slot :: path)
  def pull(): Path = Path(path.tail)

class Export(private var map: Map[Path, Local] = Map.empty) extends Equals:
  def put(path: Path, value: Local): Local = { map += (path -> value); value }
  def get(path: Path): Option[Local] = map.get(path)
  def root(): Local = get(Path()).get
  def paths: Map[Path, Local] = map

  override def equals(o: Any): Boolean = o match {
    case x: Export => x.paths == map
    case _ => false
  }

  override def canEqual(that: Any): Boolean = that match { case _: Export => true; case _ => false }

  override def hashCode(): Int = map.hashCode()

  override def toString: String = map.toString

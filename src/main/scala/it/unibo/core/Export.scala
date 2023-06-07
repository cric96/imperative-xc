package it.unibo.core

/**
 * generic local data, internal representation for raw data to be sent.
 * This will be defined in the concrete VM (e.g., they could be Rust data structures)
 */
trait Local
// No data, just a tag
case object Empty extends Local
/**
 * a specific data, a tag used for marking the tree
 * @param data the data to put in the evaluation tree 
 */
sealed case class Tag(data: String | Int | Double | Boolean | Local)
case class Slot(tag: Tag, index: Int)

case class Path(path: List[Slot] = List.empty):
  def push(slot: Slot): Path = Path(slot :: path)
  def pull(): Path = Path(path.tail)

object Path:
  def apply(slots: Slot*): Path = Path(slots.toList)

class Export(private var map: Map[Path, Local] = Map.empty):
  def put(path: Path, value: Local): Local = { map += (path -> value); value }
  def get(path: Path): Option[Local] = map.get(path)
  def root(): Local = get(Path()).get
  def paths: Map[Path, Local] = map

  override def equals(o: Any): Boolean = o match {
    case x: Export => x.paths == map
    case _ => false
  }

  override def hashCode(): Int = map.hashCode()

  override def toString: String = map.toString

package it.unibo

import it.unibo.core.{Local, Tag}

object util {
  case class RawLocal(data: Any) extends Local
  given writer[A]: Conversion[A, Local] = RawLocal(_)
  given reader[A]: Conversion[Local, A] = {
    case RawLocal(data) => data.asInstanceOf[A]
    case Tag(tag) => tag.asInstanceOf[A]
  }
}

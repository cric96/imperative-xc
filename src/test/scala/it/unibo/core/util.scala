package it.unibo.core

object util {
  case class RawLocal(data: Any) extends Local
  given Conversion[Any, RawLocal] = RawLocal(_)
  given Conversion[RawLocal, Any] = _.data
}

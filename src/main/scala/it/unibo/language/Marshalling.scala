package it.unibo.language

import it.unibo.core.Local

type Writer = [A] =>> Conversion[A, Local]
type Reader = [A] =>> Conversion[Local, A]

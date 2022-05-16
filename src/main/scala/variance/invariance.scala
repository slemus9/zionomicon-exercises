package variance

import zio.UIO

object InvarianceExamples {

  /* 
    Sometimes we use invariance because the type 
    parameter is used both as an input and output
  */
  trait InvariantRef [A] {

    def get: UIO[A]
    def set (a: A): UIO[Unit]
  }

  /* 
    We could have two type parameters to represent inputs 
    and outputs.

    This also allows us to define a map (to transform the input type)
    and a contramap (to transform the output type). However having this 
    two type parameters adds more complexity
  */
  trait ZRef [-A, +B] {

    def set (a: A): UIO[Unit]
    def get: UIO[B]
  }

  type Ref [A] = ZRef[A, A] 

}
package variance

import zio._
import zio.clock.Clock
import zio.console.Console

/* 
  Contravariant means that the subtyping relation for 
  a type A is the oposite of the relation for type F[A] 

  Contravariant types with respect to A can only consume A values to produce other values
  or effects, but can never produce A values
*/  
object ContravarianceExamples {

  trait Fruit

  trait Apple extends Fruit
  trait Orange extends Fruit

  trait Drink

  trait FoodProcessor [-Ingredient] {

    def process (ingredient: Ingredient): Drink
  }

  trait OrangeJuice extends Drink
  trait Smoothie extends Drink 

  /* 
    Orange <: Fruit => FoodProcessor[Fruit] <: FoodProcessor[Orange]

    Then every time we need a FoodProcessor[Orange], we can use a FoodProcessor[Fruit],
    which makes sense.

    Some other examples: 

      - A => B is contravariant with respect to A, because it consumes A values to produce B values
      - ZIO[R, E, A] is contravariant with respect to R (Resource), because it needs it to produce Either[E, A]
      - ZSink is contravariant with respecto to I (Input), because it needs it to produce some output
  */
  val juicer = new FoodProcessor[Orange] {

    def process (ingredient: Orange): Drink = new OrangeJuice {}
  }

  val blender = new FoodProcessor[Fruit] {

    def process (ingredient: Fruit): Drink = new Smoothie {}
  }


  val nanoTime: URIO[Has[Clock], Long] = ???

  def printLine (line: String): URIO[Has[Console], Unit] = ???

  /* 
    With contravariance we get a natural composition of effects.
    since nanoTime requires Clock and printLine requires console,
    printTime requires Clock with Console.

    Without contravariance, the types of nanoTime and printLine would 
    have been seen unralated, and we wouldn't be able to compose them
    easily. Some libraries that do not use contravariance usually define
    a function called `narrow`, which is analogous to `widen`
  */
  val printTime: URIO[Has[Clock] with Has[Console], Unit] =
    nanoTime.map(_.toString) >>= printLine

  trait ZIO [-R, +E, +A] {

    /* 
      This is essentially saying that, if we have an effect that requires
      some services, and another one that requires additional services,
      the combined effect would also require those additional services
    */
    def flatMap [R1 <: R, E1 >: E, B] (
      f: A => ZIO[R1, R1, B]
    )
  }
}
package variance

import zio._

/* 
  Covariance means that the subtyping relation for 
  a type A is the same as the relation for type F[A] 

  Covariant types F[+A] are generally types that contain A values
  or can generate A values

  Covariant types with respect to A can only produce A values but
  can never consume them
  
  if Collection[+A] is covariant with respect to A
  Cat <: Animal => Collection[Cat] <: Collection[Animal]
*/  
object CoVarianceExamples {

  trait Gen[-R, +A] {

    def sample: ZIO[R, Nothing, A]

    /* 
      The following does not compile: 
      
      def contains (a: A): ZIO[R, Nothing, Boolean]

      Covariant type A occurs in contravariant position, since the Function type
      is contravariant with respect to its argument.

      This shows that with convariance, Gen can only produce A but not consume it:

        If A <: B then Gen[R, A] <: Gen[R, B]. Then, for example, the following is true:

        Gen[Has[Random], Left[String, Nothing]] <: Gen[Has[Random], Either[String, Int]]

        But accepting in the contains function an argument of type Either[String, Int] in
        place of an argument of type Left[String, Nothing] would generate runtime errors,
        since it would be valid to pass in a Right instead of a Left.
      
      We can, however, define contains in terms of a supertype of A (which could be any value,
      so the function has to handle that situation)
    */
    def contains [A1 >: A] (a: A1): ZIO[R, Nothing, Boolean]
  }

  final case class Collection [+A] (elements: List[A]) {

    /* 
      This is essentially saying that if we have a Collection[A],
      we can combine it, not only with other values of type Collection[A],
      but also with other Collection's of a more general type A1, but then that
      would result in a Collection[A1]
    */
    def concat [A1 >: A] (that: Collection[A1]) = Collection(
      this.elements ::: that.elements
    )
  }

  val cats = Collection(List(Cat("spots"), Cat("mittens")))
  val dogs = Collection(List(Dog("fido"), Dog("rover")))

  val catsAndDogs: Collection[Animal] = cats.concat(dogs)
}

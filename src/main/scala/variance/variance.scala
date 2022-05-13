package variance

// Variance refers to how the subtyping relationship for 
// a parameterized type relates to the subtyping relationship
// for the type on which is parameterized
trait Animal {
  val name: String
}

// Cat <: Animal
final case class Cat (name: String) extends Animal

// Dog <: Animal
final case class Dog (name: String) extends Animal

object VarianceExamples {


  // By default parameterized types are invariant, so the subtyping 
  // relationship of the type A has nothing to do with the subtyping
  // relationship of type F[A]. In this case Collection[Cat] is unrelated
  // to Collection[Animal]
  final case class Collection [A] (elements: List[A])

  object Collection {

    def combine [A] (xs: Collection[A], ys: Collection[A]): Collection[A] = Collection(
      xs.elements ::: ys.elements
    )

    // workaround for types that do not use declaration site variance
    def widen [A, B >: A] (collection: Collection[A]): Collection[B] =
      Collection(collection.elements)
  }

  val cats = Collection(List(Cat("spots"), Cat("mittens")))
  val dogs = Collection(List(Dog("fido"), Dog("rover")))

  // does not compile
  // cats.combine(dogs)
  
  // workaround
  val catsAndDogs = Collection.combine(
    Collection.widen[Cat, Animal](cats),
    Collection.widen[Dog, Animal](dogs)
  )
}
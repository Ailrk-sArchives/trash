#include <iostream>
#include <type_traits>
// https://www.researchgate.net/publication/221241323_A_comparison_of_C_concepts_and_Haskell_type_classes
// property based generic programming.
// Generic programming aims to abstract away parts in the program that are
// repetitive, allow us to write the most generic version of a program that
// can be instantiated to specific types.

// A programming language has terms, expression so to speak
// There are inherent types of each term. If you make them explicit it's a
// staticly typed langauge.
//
// Then if we have a function that takes different types but works the same,
// we can abstract the function away, represents those types as type paramter.
// this is the simpliest parametric polymorphism with full paramatricity.
//
// Ok from now, how do we do generic programming?
//
// Generic programming requires concepts and models. A concept is something that
// non concrete descrption of the behavior of a set of things. The model is
// a concrete realization of the behavior for one of the thing that conforms the
// concept.
//
// E.g We have an idea of something that can be equal with each other. There are
// lots of things that conform this requirement: integer can be equal to another
// integer, my name can be equal to your name if they comprises the same
// sequence of characters...
// On the other hand, there are things that can't be compared with equality, or
// at least in a sense they can't be compared. Apples, for example. Are there
// two apples in the world that are identical? It can't bem other wise they are
// the same apple... Or in programming, if two lambda terms describes the same
// computation, can we say these two terms are exactly the same? Maybe not,
// because they are inherently different.
//
// Name is a type that supports equality, apple is a type that doesn't support
// equality. We have the concept in our mind very clearly. We need some how
// to elaborate them with a proper language.
//
// To describe the concept, something we need to care about:
// 1. What's the name of the concept?
// 2. What makes the concept this concept? (definition of the concept)
// 3. What is a thing that conforms this concept?
// 4. How does the thing conforms the concept?

// E.g to describe equality, we do the following:
// 1. We have a concept `Eq` for equality.
// 2. Anything than conforms equality needs to check if it `equals to` another
//    things.
// 3. Integer conforms equality, we say 1=1 but 1 != 0
// 4. How do we know two integers are equal? Say we have integer a and b,
//    if a - b = 0 then they are equal.

// The above descrption is an informal attempt to describe the concept.
// Without concepts, we can only talk about specific things, lots of lots of
// things that are clearly similar to each other, but we can't describe the
// similarity. You can drink water, you can drink wine. but there is nothing
// like drinkable that allows you to descirbe things can be drank in general.

// It's not like generic programming is the only way to achieve abstraction,
// there are many other ways. A function is a abstraction, a variable is an
// abstraction. But with this basic elements alone, we are lack of an
// langauge that describes something that coforms the combination of
// smaller abstraction mechanisms.

// Of cousrse generic programming is a method of programming, so we need to
// work with programming languages and programming concepts.
// Functoins, values at term level, types at type level. These are basic
// ingredient.

// First we describe there is a concept Eq. class Eq a where
//  what's Eq? something is Eq. that something needs to be a type of kind Type.
// Then we can describe what makes a thing Eq. we say eq :: a -> a -> Bool
// Then we can describe a type is a member of a concept as integer is in Eq.
//  instance Eq Integer where
// Then we can describe integer is in Eq
//  eq a b = a - b == 0

// This is the basic construct. But we also may have a concept that concerns
// multiple types, or a concept that describes the relatoinship between two
// specific types, etc.

// E.g you might want to have a memoized function. A memoized function will
// memoized the mapping relationship between it's parameter and computed output.
// The concept in our mind is something memoizable. But if something is
// memoizable, there is a specific type of table that store the memoization.
// if you meoize int to int it's simple, just Map int Int. if you memoize
// from (Int, Int, Username, ...) to Json, a table with different shape is
// required.
// The table type associate with the concept. To better support generic
// programming we need to support this.

// E.g when describing a general relationship between things, your concept
// may need multiple type paramters. ...

// concepts doesn't necessarily need to have concrete representation. Sometimes
// is suffice to just make it a type level predicate, which is just a type level
// function that return true for types that "models" the concept.

// Eight features of generic programming
// 1. Multi type concepts
// 2. multiple constraints
// 3. associated type access
// 4. retroactive modeling
// 5. type aliases
// 6. separate compilation
// 7. implicit instantiation
// 8. concise syntax

// The core idea is concept and model. How to integrate them into the langauge
// is something else we need to care about. If a generic programming feature
// fails to deliver easy descriptions of concept and model, it's not a good
// feature.

//////////////////////////////////////
// concepts help compiler to select appropriate overload.
// Intent of concept is to model semantic catagory. As type parameter T belongs
// to a set of types.

//////////////////////////////////////
// Concepts in c++
// Let's see before c++20 concept how does c++ supports generic programming:
//
// - parametric polymorphism with template.
// - use subtyping or documentation to describe a concept.
// - type level function with type traits
//   - use type trait to get assoicate types.
//   - use type trait to get type level predicate.
// - tempalte specialization and partial specialization for modeling concept.
// - constraint type parameter with type traits and sfinae.

// Really the biggest gain is the concept of concept becomes much cleaner and
// easier to write. Tempate was powerful enough to describe most of generic
// programming features, but most of them are hacks.

// Note: some weird type traits like std::is_copyable<T> is
// also a concept. It's just a type level concept that the model is trivial.
// Or maybe you can say the model is inherent from the lower level feature of
// the type.

// With C++ concept we can:
// - Type level constraint instead of sfinae. (synatic improvement)
// - Don't need subtyping or documentation to describe a constraint.
// - Refine concept with other concepts. (sytatic improvement.
//     We can conjunct type traits anyway.)

//////////////////////////////////////
// define concept.
// Concept will check the description and reject ilegal input accordingly.
// It's another system build on top of the duck typing based template
// paramter.
template <typename T> // require introduce a type level predicate.
concept Decremental = requires(T t) {
  --t;
};

// The concept Decremental `refines` RevIterator.
// Retoractive modeling
template <typename T>
concept RevIterator = Decremental<T> && requires(T t) {
  *t;
};

template <typename T, typename U>
concept Derived = std::is_base_of<U, T>::value;

template <typename T>
concept HasIterator = requires {
  typename T::iterator; // iterator shoule be a typename
};

template <typename T> // the entire require clause works the same as P(a)
concept Hashtable = requires(T a) {
  { std::hash<T>{}(a) } -> std::convertible_to<size_t>;
};

template <typename T>
concept Addable = requires(T a, T b) {
  {a + b};
};

template <typename T, typename U = T>
concept Swappable = requires(T &&t, U &&u) {
  swap(std::forward<T>(t), std::forward<U>(u));
  swap(std::forward<U>(u), std::forward<T>(t));
};

// Full Parametricity
template <typename T>
concept Tautology = true;

//////////////////////////////////////
// use constraints.

template <Decremental T> T bar(T t) { return --t; }
template <RevIterator T> T foo(T t) { return *t; }
template <HasIterator T> T gaw(T t) { return t; }
template <Hashtable T> T hashit(T t) { return std::hash<T>{}(t) + 1; }

// need top provide Base
template <typename Base, Derived<Base> T> std::string issubtype(T t) {
  return "Yeah";
}

struct Dummy {
  using iterator = int;
  std::string to_string() { return "I'm dummy"; }
};

struct LilDummy : public Dummy {};

//////////////////////////////////////
int main(void) {

  Dummy d{};

  std::cout << "derement constraint: " << bar(10) << std::endl;
  std::cout << "hash constraint: " << hashit(10) << std::endl;

  // oh nice this compiles now.
  std::cout << gaw(Dummy()).to_string() << std::endl;
  return 0;
}

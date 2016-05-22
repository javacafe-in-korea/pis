// http://booksites.artima.com/programming_in_scala_2ed/examples/html/ch19.html

trait Queue[T] {
  def head: T

  def tail: Queue[T]

  def enqueue(x: T): Queue[T]
}

object Queue {

  def apply[T](xs: T*): Queue[T] =
    new QueueImpl[T](xs.toList, Nil)

  private class QueueImpl[T](
                              private val leading: List[T],
                              private val trailing: List[T]
                            ) extends Queue[T] {

    def mirror =
      if (leading.isEmpty)
        new QueueImpl(trailing.reverse, Nil)
      else
        this

    def head: T = mirror.leading.head

    def tail: QueueImpl[T] = {
      val q = mirror
      new QueueImpl(q.leading.tail, q.trailing)
    }

    def enqueue(x: T) =
      new QueueImpl(leading, x :: trailing)
  }

}


class StrangeIntQueue extends Queue[Int] {
  override def enqueue(x: Int) = {
    println(math.sqrt(x))
    enqueue(x)
  }

  override def head: Int = {0}

  override def tail: Queue[Int] = {Queue(1)}
}

// val x: Queue[Any] = new StrangeIntQueue
// x.enqueue("abc")


// Variance

class Animal { val sound = "rustle" }
class Bird extends Animal { override val sound = "call" }
class Chicken extends Bird { override val sound = "cluck" }

// type of parameters of function is contravariant
val getTweet: (Bird => String) = {
  def f(a:Animal) = a.sound
  f
}
getTweet(new Bird)
// getTweet(new Animal) error
getTweet(new Chicken)

// return type is covariant
val hatch: (() => Bird) = () => new Chicken

// https://twitter.github.io/scala_school/ko/type-basics.html
def foo[A, B](f: A => List[A], b:A) = f(b)

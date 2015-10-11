package ts

import Stream._
trait Stream[+A]{
	 def take2(n: Int): Stream[A] = this match {
    	case Cons(h, t) if n > 1 => cons(h(), t().take2(n - 1))
    	case Cons(h, _) if n == 1 => cons(h(), empty)
    	case _ => empty
  	}

  	def toList: List[A] = {
	val buf = new collection.mutable.ListBuffer[A]
	def go(l: Stream[A]): Unit = l match { //go[A] doesnt compile???
		case Empty => Empty
		case Cons(h,t) => 
			buf += h()
			go(t())
			
	}
	go(this)
	buf.toList
	
	}

}

case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] //non empty stream with a non strict head and tail


object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)
}

package ch5

import Stream._
trait Stream[+A]{

	def foldRight[B](z: => B)(f: (A, => B) => B): B=
		this  match {
			case Cons(h,t) => f( h() , t().foldRight(z)(f))
			case _ => z
		}

	def exists( p: A => Boolean):Boolean = {
		foldRight(false)( (a,b) => p(a) || b)
	}
	//5.1
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

	def toListFast: List[A] = {
	  val buf = new collection.mutable.ListBuffer[A] 
	  @annotation.tailrec
	  def go(s: Stream[A]): List[A] = s match {
	    case Cons(h,t) =>
	      buf += h()
	      go(t())
	    case _ => buf.toList
	  }
  	go(this)
	}
	//5.2
	def take(n: Int): Stream[A] = {
		def go(c: Int, s: => Stream[A]): Stream[A] = {
			s match {
				case Cons(h,t) if c> 1 => cons(h(), go( c-1,t() ) )
				case Cons(h,t) => cons(h(),empty)
				case _ => empty

			}
		}
		go(n,this)

	}

	 def take2(n: Int): Stream[A] = this match {
    	case Cons(h, t) if n > 1 => cons(h(), t().take2(n - 1))
    	case Cons(h, _) if n == 1 => cons(h(), empty)
    	case _ => empty
  	}	
	//5.2 drop
	@annotation.tailrec
	final def drop(n: Int): Stream[A] = { //why final for tail rec
		this match {
			case Cons(h,t) if n>1 => t().drop(n-1)
			case Cons(h,t) => t()
			case _ => empty
		}
	}
	//5.3
	
	def takeWhile(p: A=> Boolean): Stream[A] = this match {
		case Cons(h,t) if p(h())=> cons(h(),t().takeWhile(p))
		case _ => empty 
	}
	//5.4
	def forAll(p: A => Boolean): Boolean = {
		foldRight(true)(( a,b) =>  p(a) && b  )
	}
	//5.5
	def takeWhileFR(p: A => Boolean): Stream[A] = {
		foldRight( Stream[A]() )( (a,b) => {if(p(a)) cons(a,b) else empty  } )
	}
	//5.6
	def headOption: Option[A] ={
		foldRight( None: Option[A] )( (a,b) => Some(a) )

	} 
	//5.7
	def map[B](f: A => B): Stream[B] = 
		foldRight(Empty: Stream[B])( ( a,b) => cons( f(a), b) )

	def filter(f: A => Boolean): Stream[A] = 
		foldRight(empty[A])( (a,b) => if(f(a)) cons(a,b) else b)

	def append[B>:A](s: => Stream[B]): Stream[B] = 
		foldRight(s)(cons(_,_))

	def flatMap[B](f: A => Stream[B]): Stream[B] =
		foldRight(empty[B])( (a,b) => f(a).append(b) )
	//5.13
	def mapUnfold[B](f: A => B ): Stream[B] =
		unfold( this )( { case Cons(hh,tt) => Some( (f(hh()),tt() ) )})

	def takeUnfold(n: Int): Stream[A] = 
		unfold( (this,n))( {

			case (Cons(h,t),x) if(x!=0) => Some( (h(),(t(),x-1))) 
			case _ => None
			})
	def takeWhileUnfold(p: A => Boolean): Stream[A] = 
		unfold(this)( { 
			case Cons(h,t) if (p(h())) => Some(h(),t()) //h is evaluated twice?
			case _ => None })

	def myzipWith[B,C](b: Stream[B])(f: (A,B)=> C): Stream[C] =
		unfold( (this,b) )( { 
			case (Cons(h1,t1),Cons(h2,t2)) => Some( (f(h1(),h2()) , (t1(),t2())))
			})
	def myzipAll[B](b: Stream[B]): Stream[(Option[A],Option[B])] = 
		unfold( ( this,b) )( {
			case (Cons(h1,t1),Cons(h2,t2)) => Some(  ((Some(h1()),Some(h2())) ,(t1(),t2())) )
			// case (Cons(h1,t1),_) => None
			// case (_,Cons(h,t)) => None
			case(_,_) => None
			})
	def zipAll[B](s2: Stream[B]): Stream[(Option[A],Option[B])] =
  	zipWithAll(s2)((_,_))

	def zipWithAll[B, C](s2: Stream[B])(f: (Option[A], Option[B]) => C): Stream[C] =
  	Stream.unfold((this, s2)) {
	    case (Empty, Empty) => None
	    case (Cons(h, t), Empty) => Some(f(Some(h()), Option.empty[B]) -> (t(), empty[B]))
	    case (Empty, Cons(h, t)) => Some(f(Option.empty[A], Some(h())) -> (empty[A] -> t()))
	    case (Cons(h1, t1), Cons(h2, t2)) => Some(f(Some(h1()), Some(h2())) -> (t1() -> t2()))
  	}
	// 5.14
	// def startsWith[A](s: Stream[A]): Boolean = {
	// 		def go(a: Stream[A],b: Stream[A]): Boolean = 
	// 		(a,b) match {
	// 			case (_,Empty) => true
	// 			case (Cons(h,t),Cons(sh,st)) if (h==t) => go(t,st)
	// 			case (Empty,_) => false
	// 		}
	// 		go(this,s)
	// 	}
	def sw[A](s: Stream[A]): Boolean = 
		this.zipAll(s).takeWhile( x=> x._1==x._2) == s

	def startsWith[A](s: Stream[A]): Boolean = 
  		zipAll(s).takeWhile(!_._2.isEmpty) forAll {
    		case (h,h2) => h == h2
  	}
  	//5.15
  	def tails: Stream[Stream[A]]=
  		unfold(this)( {
  			case Cons(h,t) => Some((Cons(h,t),t()))
  			case _ => None
  			})
  	//5.16
  	def scanRight[B](z: => B)( f: (A, =>B)=> B): Stream[B] = {
  		def go(a: Stream[A])(z: => B , f: (A,=>B)=> B): Stream[B] =
  			a match {
  				case Cons(h,t) => Cons(() => a.foldRight(z)(f), () => go(t())(z,f))
  				case _ => empty
  			} 
  		go(this)(z,f)
  	}
		
		
	
	
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A] //non empty stream with a non strict head and tail


object Stream {
	def cons[A](hd: => A,tl: => Stream[A]): Stream[A] = {
		lazy val head =hd
		lazy val tail = tl
		Cons( () => head, () => tail)
	}
	def empty[A]: Stream[A] = Empty

	def apply[A](as: A*): Stream[A]=
	 if(as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

	//mine val ones: Stream[Int] = Stream.cons(1,ones)
	val ones: Stream[Int] = Stream.cons(1, ones)
	// //5.8
	def constant[A](a: A): Stream[A]={
		lazy val tail: Stream[A] = Cons(()=>a , ()=>tail)
		tail
	}

	def constantLazy[A](a: A): Stream[A]={
		cons(a,constant(a))
	}
	//5.9
	def from(n: Int): Stream[Int]={
		cons(n,from(n+1))
	}
	//5.10 real answer doest work for me
	val fib ={
		def go(p:Int , c: Int): Stream[Int] ={
			cons(p,go(p,p+c))	
			//go( s.append(Stream(c+p) ),c,c+p) 
		}
		go( 0,1 )
	}

	val fibs = {
 		def go(f0: Int, f1: Int): Stream[Int] = 
    		cons(f0, go(f1, f0+f1))
  	go(0, 1)
	}
	
	//5.11
	def myunfold[A,S](z: S)(f: S => Option[ (A,S)] ): Stream[A] = {
		cons(f(z).get._1,myunfold(f(z).get._2)(f))
	}
	def unfold[A,S](z:S)(f:S => Option[(A,S)]): Stream[A] ={
		f(z) match {
			case Some((h,s)) => cons(h,unfold(s)(f))
			case None => empty
		}
	}
	//5.12
	def fromUnfold(n:Int): Stream[Int] = {
		unfold(n)( x=> Some(x+1,x+1))
	}

	val onesUnfold :Stream[Int] = 
		unfold(1)(_=>Some(1,1))

	val fibsUnfold =  //had right idea , struggled with syntax
		unfold( (0,1) )( { case (p,c) => Some(p,(c,(p+c) ) )})
	def constantUnfold[A](x: A): Stream[A] =
		unfold(x)(_=>Some(x,x))

			
}

object test{
	def if2[A](cond: Boolean , onTrue: => Int, onFalse: => Int): Int={
		lazy val t = onTrue
		if(cond) {t +t } else onFalse
	}
}
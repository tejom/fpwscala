package fpinscala.datastructures

sealed trait List[+A] //define datatype trait interfac that contains implentation of methods

case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]


object List {
	def sum(ints: List[Int]): Int = ints match {
		case Nil => 0
		case Cons(x,xs) => x +sum(xs)
	}

	def apply[A](as: A*):List[A]=
		if (as.isEmpty) Nil
		else Cons(as.head,apply(as.tail: _*))

	def check(is: List[Int]):Int = is match {
	case Nil => 4
	case Cons(4,h) => 1000
	case Cons(t,h) => head(h)
	case _ => 100
	
	}

	def head[A](as: List[A]):A = as match {
		
		case Cons(h,Nil) => h
		case Cons(a,as) => a

	}
	def getLast[A](as: List[A]):A = as match {
		case Cons(h,Nil) => h
		case Cons(b,bs) => getLast(bs)

	}
	def foldRight[A,B](l: List[A], z: B)(f: (A,B)=> B) : B =
		l match{
			case Nil => z
			case Cons(x,xs) => f(x, foldRight(xs,z)(f))
		}

	def sum2(l: List[Int]) = 
		foldRight(l,0)(_ + _)
	 //exercise2
	 def tail[A](as: List[A]):List[A] = as match {
	 	case Nil => Nil
	 	case Cons(h,hs) => hs
	 }
	 //exercise3
	 def drop[A](as: List[A], n: Int):List[A] = {
	 	if(n==0) as
	 	else {
	 		as match  {
	 			case Nil => Nil
		 		case Cons(h,hs) => drop(hs,n-1) 
		 	}
		 	
		}
	 	
	 }
	 //exercise 4
	 def dropWhile[A](l: List[A])(f: A=> Boolean): List[A] = l match {
	 	case Nil => Nil
	 	case Cons(a,as) => {
	 		if( f(a) ) dropWhile(as)(f)
	 		else l
	 	}
	 }
	 //exercise5
	 def setHead[A](l: List[A] , a: A): List[A] = l match{
	 	case Nil => Nil
	 	case Cons(x,xs) => Cons(a,xs)
	 }
	 //exercise6
	 def init[A](l: List[A]): List[A] = l match{
	 	case Cons(h,Nil)=> l
	 	//case Cons(h,Nil)=> Nil
	 	case Cons(h,hs) => Cons(h,init(hs) )
	 }
	 //exercise7 - no the recursion is being done within foldright
	 def product2(l: List[Double]) = 
	 	if(head(l)==0.0) 1000.0
	 	else foldRight(l,1.0)(_*_)
	 //exercise 8

	 //exercise 9
	 def length[A](l: List[A]): Int = {
	 	def loop[A](x: List[A],c: Int): Int = x match {
	 		case Nil => c
	 		case Cons(y,ys) => loop(ys,c+1)
	 	}
	 	loop(l,0)
	}
	def length2[A](l: List[A]): Int = {
	 	foldRight(l,0)( (x,c: Int) => l match{
	 		case Cons(y,Nil) => 1
	 		case Cons(y,ys) => c+1
	 		})
	}
	def length22[A](l: List[A]): Int = {
	 	foldRight(l,0)( (_,c: Int) => 
	 		 c+1
	 		)
	}
	def length3[A](l: List[A]):Int =
		foldRight(l, 0)((_,acc) => acc + 1)
	// exercise10
	def myfoldLeft[A,B](l :List[A],z: B)(f: (B,A)=>B): B = {
		@annotation.tailrec
		def go(x: List[A]): B= 
			x match {
				case Cons(y,Nil) => f(z,y)
				case Cons(y,ys) => {
					go(ys)
					
				}


			}
		go(l)
		
	}
	@annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match { 
	  case Nil => z
	  case Cons(h,t) => foldLeft(t, f(z,h))(f)
	}
	//exercise 11
	def leftSum(l: List[Int]):Int =
	foldLeft(l,0)(_+_)

	def leftProduct(l: List[Double]):Double = 
		foldLeft(l,0.0)(_*_)

	def leftLength[A](l: List[A]): Int =
		foldLeft(l,0)( (c:Int,_ )=> c+1) 

	//exercise12
	def reverse[Int](l: List[Int]): List[Int] = l match {
		case Cons(x,xs) => foldLeft(xs,Cons(x,Nil))((a,b) => Cons(b,a))


	}
	def reverse2[A](l: List[A]): List[A] =
	 foldLeft(l, List[A]()) ((a,b) => Cons(b,a))

	//exercise13
	//def newFoldRight

	//exercise14
	def append[A](l: List[A] , x: List[A]): List[A]=
		foldRight(l,x)(Cons(_,_))



	



}
sealed trait test

case class Func(a: Int,b: Int) extends test

object test {
	def apply(a: Int ,b: Int): Int ={
		a+b
	}
	def minus(a: Int,b:Int): Int = {
		a-b
	}
	
		
	
}
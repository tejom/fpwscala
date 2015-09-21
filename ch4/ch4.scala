package ch4
sealed trait Option[+A] {
	def map[B](f: A=> B):Option[B] = this match {
		case Some(i) => Some(f(i))
		case _ => None
	}
	
	def flatMap[B](f: A => Option[B]): Option[B] = {
		map(f).getOrElse(None)
		//dont understand this answer
	}
	def getOrElse[B >: A](default: => B):B = this match{
		case Some(i) => i
		case _ => default
	}
	def orElse[B >: A](ob: => Option[B]): Option[B] = {
	 	//if(ob == None) ob
	 	//else this
	 	//didnt understand this question
	 	this map (Some(_)) getOrElse ob
	 }
	def filter(f: A => Boolean): Option[A] =
		if( this.map(f) == Some(true))  this
		else None
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object ex {
	//4.2
	def variance(xs: Seq[Double]): Option[Double] = {
		
		mean(xs) flatMap (m => mean( xs.map( x => math.pow(x - m ,2.0)) ) )



	}
	//4.3
	def mean(xs: Seq[Double]): Option[Double] = 
		if (xs.isEmpty) None
		else Some(xs.sum /xs.length)

	def map2[A,B,C](a: Option[A], b: Option[B])(f: (A,B)=> C): Option[C] = {
		(a,b) match {
			case (Some(x),Some(y)) => Some( f (x,y))
			case (_,_) => None
		}
	}
	def map2answer[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
 	 a flatMap (aa => b map (bb => f(aa, bb)))
	//4.4
	def mysequence[A](a: List[Option[A]]): Option[List[A]] = {
		a match {
			case x :: xs if x ==None => Some(Nil)
			case x :: xs=>  x.flatMap( xx => sequence(xs).map(xx::_)) 
		}
	}

	def sequence[A](a: List[Option[A]]): Option[List[A]] =
      a match {
        case Nil => Some(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
    }

    def sequenceFor(a: Option[Int]): Option[Int] = {
    	for {
    			aa <- a
    		} yield ( aa *2)
    	
    }
    //4.5
    def traverse[A,B](a: List[A])(f: A => Option[B]): Option[List[B]] ={
    	a match {
        case Nil => Some(Nil)
        case h :: t => f(h) flatMap (hh => traverse(t)(f) map (hh :: _))
        }
    }



}

sealed trait Either[+E, +A] {
	def map[B](f: A => B): Either[E,B] = this match {
			case Right(i) => Right( f(i))
			case Left(i) => Left(i)
		}
	
	def flatMap[EE >: E , B](f: A => Either[EE,B]): Either[EE,B] = {
		this match {
			case Right(i) => f(i)
			case Left(i) => Left(i)
		}
	}

	def orElse[EE >: E, B >: A](b: Either[EE,B]): Either[EE,B] = {
		this match {
			case Left(_) => b
			case Right(i) => Right(i)
		}
	 }

	 def map2[EE >: E,B,C](b: Either[EE,B])(f: (A,B)=> C): Either[EE,C] ={
	 	this match {
	 		case Left(i) => Left(i)
	 		case Right(_) => this.flatMap( tt => b.map( bb => f(tt,bb)))
	 	}
	 }
	 def map22[EE >: E,B,C](b: Either[EE,B])(f: (A,B)=> C): Either[EE,C] ={
	 	 for( 
	 			tt <- this;
	 		 	bb <- b
	 		 	) yield f(tt,bb)
	 	
	 }
	 def map2answer[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): 
 		 Either[EE, C] = for { a <- this; b1 <- b } yield f(a,b1)

 	def sequence[E,A](es: List[Either[E,A]]): Either[E,List[A]]={
 		es match{
 			case Nil => Right(Nil)
 			case h :: t => h.flatMap(hh => sequence(t).map(hh :: _))
 		}
 	}

 	def traverse[E,A,B](es: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
 		es match{
 			case Nil => Right(Nil)
 			case h :: t => f(h).flatMap( hh => traverse(t)(f).map( hh :: _))
 		}
 	}

}
case class Left[+E](value: E) extends Either[E,Nothing]
case class Right[+A](value: A) extends Either[Nothing,A]
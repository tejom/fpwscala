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
	//4.4

}
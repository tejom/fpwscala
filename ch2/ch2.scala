object ch2module {


}

object fibNumbers {
  
  def fib(n: Int): Int = {
      if(n<2) n
      else fib(n-1) + fib(n-2)
  }
}

object sort {
  def isSorted[A](as: Array[A],gt: (A,A) => Boolean): Boolean = {
    def go[A](c: Int): Boolean = {
      if (c+1 == as.length) true
      else if (gt(as(c),as(c+1))) go(c+1)
      else false
    }
    go(0)
  }  
}

object exercise3 {
  def partial1[A,B,C](a: A,f: (A,B) => C) : B => C = {
   (b: B) => f(a,b)
  }
  
}

object exercise4 {
  def curry[A,B,C]( f: (A,B)=>C): A => ( B => C) = 
   a =>  b => f(a,b) 

   
}

object test {
  def rf[A,B,C] (f: (A,B)=>C): A => B => C ={
    (a: A) => (b: B) => f(a,b)
    
  }

  def un[A,B,C](f: A => B => C): (A,B) => C =
  (a:A , b:B )=> f(a)(b)

  def compose1[A,B,C](f: B=> C, g: A=> B): A => C =
    a => f(g(a))

}


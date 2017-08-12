
object pairs {

  def isPrime(n:Int):Boolean=

    (2 until n) forall (x=>x%n!=0)
//prime number is a number which is divisible by itslf and
  val n = 10
  (1 until n) flatMap (i => (1 until i) map (j => (i, j))) filter (pairs =>
    isPrime(pairs._1 + pairs._2))


  val s = List(1.0,2,3,4,5,6,7)
  val d = List(2.9,3,4,5,6,7,8)


  def scalarProduct(xs: List[Double],ys:List[Double]) : Double=
    ( for {
    (x,y)<- xs zip ys
} yield (x*y) )sum

scalarProduct(s,d)


}
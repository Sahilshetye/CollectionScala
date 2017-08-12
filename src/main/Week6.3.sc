object nquens{
  def queens(n:Int):Set[List[Int]]={
     def placequeens(k:Int):Set[List[Int]]=
      if(k==0) Set(List())
      else
        for{
          queens<- placequeens(k-1)
          col <- 0 until n
          if issafe(col,queens)
        } yield col :: queens
    placequeens(n)
  }
 def issafe(col:Int,queens:List[Int]):Boolean= {
   val row = queens.length
    val queenswithrow=((row -1) to 0 by -1) zip queens
   queenswithrow forall{
     case(r,c) => col!=c && math.abs(col-c ) != row - r

   }
 }
def show(queens:List[Int])={
  val lines=
    for (col <- queens.reverse)
      yield Vector.fill(queens.length)("* ").updated(col,"X ").mkString
  "\n"+ (lines mkString "\n")
}
  (queens(5) take 3 map show) mkString("\n")

}
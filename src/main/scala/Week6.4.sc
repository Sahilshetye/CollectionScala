object polynomial{
//This method with concat ++
//  class pol(terms0:Map[Int,Double]){
//    def this(binding:(Int,Double)*)= this(binding.toMap)
//    val terms= terms0 withDefaultValue 0.0
//    def +(other:pol)=new pol(terms ++ (other.terms map adjust))
//    def adjust(term:(Int,Double)):(Int,Double)={
//      val(exp,coff)=term
//      exp->(coff+terms(exp))
//      //      terms get exp match{
//      //        case Some(coff1)=> exp->(coff+coff1)
//      //        case None => exp->coff
//      //      }
//    }
//    override def toString: String =
//    (for((exp,coff)<-terms.toList.sorted.reverse)yield coff+"x^"+exp) mkString "+"
//  }
//
//  val p1= new pol(1->2.0,3->4.0,5->6.2)
//  val p2= new pol(0->3.0,3->7.0)
//  p1 + p2
//

//  The Method with foldleft

class pol(terms0:Map[Int,Double]){
  def this(binding:(Int,Double)*)= this(binding.toMap)
  val terms= terms0 withDefaultValue 0.0
  def +(other:pol)=new pol((other.terms foldLeft terms)(addterm))
  def addterm(terms:Map[Int,Double],term:(Int,Double)):Map[Int,Double]={
    val(exp,coff)=term
    terms + (exp->(coff+terms(exp)))
      }
  override def toString: String =
    (for((exp,coff)<-terms.toList.sorted.reverse)yield coff+"x^"+exp) mkString "+"
}

  val p1= new pol(1->2.0,3->4.0,5->6.2)
  val p2= new pol(0->3.0,3->7.0)
  p1 + p2



}
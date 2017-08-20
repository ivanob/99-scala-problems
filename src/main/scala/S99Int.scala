
class S99Int(val i: Int){
  import S99Int._

  def isPrime:Boolean = {
    if(i>0 && i<=3) true
    else if(i<=0) false
    else {
      val root = Math.floor(Math.sqrt(i)).toInt
      !(2 to root).toList.map(i % _).exists(_ == 0)
    }
  }
}

object S99Int{
  implicit def int2S99Int(i: Int): S99Int = new S99Int(i)
}
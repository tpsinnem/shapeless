/*  Modeled very much after HList and KList in the (Mark Harrah's?) series on 
    type-level programming on the Apocalisp blog. To whatever extent that's 
    different from how things are done in shapeless, I will probably migrate 
    towards the shapeless way because shapeless is what I'm now trying to 
    integrate this with. */

sealed trait CIList[+I]

final case class CICons[+I, H <: I, T <: CIList[I]](head:H, tail:T) 
  extends CIList[I] {

  def ::[J >: I, E <: J](e: E) = CICons[J, E, CIList[J]](e, this)
}

sealed class CINil extends CIList[Nothing] {
  def ::[E](e: E) = CICons[E, E, CINil](e, this)
}

object CINil extends CINil

object CIList {
  //  how do i make this work like in apocalisp HList ?
  //  - probably not possible
  //type ::[I][H <: I, T <: CIList[I]] = CICons[I,H,T]
  val :: = CICons
}

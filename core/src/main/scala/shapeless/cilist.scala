/*  Modeled very much after HList and KList in the (Mark Harrah's?) series on 
    type-level programming on the Apocalisp blog. To whatever extent that's 
    different from how things are done in shapeless, I will probably migrate 
    towards the shapeless way because shapeless is what I'm now trying to 
    integrate this with. 
    - I guess I might not be integrating very much after all for now. See 
      comments in basehlist.scala.DROPPED_FOR_NOW

- It's not working. I wonder if map() can at all be declared in the base 
  CIList trait in a way that lets it be used in both CICons and CINil. (In 
  shapeless HList all such stuff is done in a separate HListOps, via implicit 
  conversions. What OTOH did Harrah's up HList do exactly?)
  - At least part of the problem is that I think I need different return types 
    depending on whether the actual CIList object is a CICons or a CINil.
  - I guess what I should do for now is to get something working for CICons 
    and CINil [separately], and figure out afterwards what the proper way 
    would be to make things nice with the base CIList. If that sort of thing 
    is possible.
    - In the CICons case I think map() will need to be parameterized with 
      'output head' and 'output tail' types (OH, OT)
*/

sealed trait CIList[+I] {
  /*  Something something problems with covariance? Think about HListOps at 
      some point again and have a clue! */

  type MapOutHighBound <: CIList[Any]
  def map[O, OL <: MapOutHighBound](f: I => O):OL

  //def ::[J >: I, E <: J](e: E):CICons[J, E, CIList[J]]
  /*
  type MapOut[O] <: CIList[O]
  def map[O](f: I => O):MapOut[O]
  */
}

final case class CICons[+I, H <: I, T <: CIList[I]](head:H, tail:T) 
  extends CIList[I] {

  def ::[J >: I, E <: J](e: E) = CICons[J, E, CIList[J]](e, this)

  type MapOutHighBound = CICons[Any, Any, CIList[Any]]

  type Interface = I
  type Head = H
  type Tail = T

  def map[O, OL <: MapOutHighBound](f: I => O):OL = 
  //def map[O, OL <: CICons[Any, Any, CIList[Any]]](f: I => O):OL = 
    CICons[OL#Interface, OL#Head, OL#Tail](f(head), tail.map(f))

  //def map[O, OH <: O, OT <: CIList[O]](f: I => O) = CICons[O, OH, OT](f(head), tail.map(f))
  
  //type MapOut[O] = CICons[O, OH <: O, OT <: CIList[O]]
  /*  Pay close attention to what the type of tail.map(f) is supposed to be 
      and how it relates to the return type of the current map call. */
  //def map[O](f: I => O):MapOut[O] = CICons(f(head), tail.map(f))
  //def map[O, OL <: CIList[O]](f: I => O):OL = f(head) :: tail.map(f)
}

sealed class CINil extends CIList[Nothing] {
  def ::[E](e: E) = CICons[E, E, CINil](e, this)
  type MapOutHighBound = CINil
  type I = Any
  def map[O, OL <: MapOutHighBound](f: I => O):OL = CINil
}

object CINil extends CINil

object CIList {
  //  how do i make this work like in apocalisp HList ?
  //  - probably not possible
  //type ::[I][H <: I, T <: CIList[I]] = CICons[I,H,T]
  val :: = CICons
}

package mrsc

package object pfp {
  type Name = String
  type Subst[C] = Map[Name, C]
  type RawRebuilding[C] = (C, Subst[C])

  def emptyContraction[C]: Contraction[C] =
    Contraction[C](null, null.asInstanceOf[C])
  def emptySubst[C]: Map[Name, C] =
    Map[Name, C]()
}

package mrsc.pfp

import mrsc.core._

trait PFPTransformer[C] extends Transformer[C, DriveInfo[C]]
  with GraphBuilder[C, DriveInfo[C]] with DriveSteps[C] {

  type N = SNode[C, DriveInfo[C]]
  type Warning
  def findBase(g: G): Option[N]
  def drive(g: G): List[S]
  def rebuildings(whistle: Option[Warning], g: G): List[S]
  def inspect(g: G): Option[Warning]

  override def descendants(g: G): List[G] =
    findBase(g) match {
      case Some(node) =>
        List(fold(node)(g))
      case _ =>
        val whistle = inspect(g)
        val driveSteps = if (whistle.isEmpty) drive(g) else List()
        val rebuildSteps = rebuildings(whistle, g)
        (rebuildSteps ++ driveSteps) map (_(g))
    }
}

trait Driving[C] extends PFPTransformer[C] { this: PFPSemantics[C] =>
  def drive(g: G): List[S] = List(driveConf(g.current.conf))
}

trait RenamingFolding[C] extends PFPTransformer[C] with PFPSyntax[C] {
  override def findBase(g: G): Option[N] =
    g.current.ancestors.find { n => subclass.equiv(g.current.conf, n.conf) }
}

trait BinaryWhistle[C] extends PFPTransformer[C] {
  type Warning = N
  val ordering: PartialOrdering[C]
  override def inspect(g: G): Option[Warning] =
    g.current.ancestors find { n => ordering.lteq(n.conf, g.current.conf) }
}

trait UnaryWhistle[C] extends PFPTransformer[C] {
  type Warning = Unit
  def dangerous(c: C): Boolean
  override def inspect(g: G): Option[Warning] =
    if (dangerous(g.current.conf)) Some(Unit) else None
}

trait AllRebuildings[C] extends PFPTransformer[C] with PFPSyntax[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    rebuildings(g.current.conf) map { rebuild(_): S }
  }
}

trait LowerRebuildingsOnBinaryWhistle[C] extends PFPTransformer[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(_) => rebuildings(g.current.conf) map { rebuild(_): S }
    }
}

trait UpperRebuildingsOnBinaryWhistle[C] extends PFPTransformer[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) => rebuildings(upper.conf) map { rollback(upper, _) }
    }
}

trait DoubleRebuildingsOnBinaryWhistle[C] extends PFPTransformer[C] with PFPSyntax[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rebuilds: List[S] =
          rebuildings(g.current.conf) map { rebuild(_): S }
        val rollbacks: List[S] =
          rebuildings(upper.conf) map { rollback(upper, _) }
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOnBinaryWhistle[C] extends PFPTransformer[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        mutualGens(g.current.conf, upper.conf) map translate map { rebuild(_): S }
    }
}

trait UpperAllBinaryGensOnBinaryWhistle[C] extends PFPTransformer[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
    }
}

trait DoubleAllBinaryGensOnBinaryWhistle[C] extends PFPTransformer[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None =>
        List()
      case Some(upper) =>
        val rollbacks: List[S] = mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map { rebuild(_): S }
        rollbacks ++ rebuilds
    }
}

trait LowerAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPTransformer[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        val rebuilds: List[S] = mutualGens(g.current.conf, upper.conf) map translate map { rebuild(_): S }
        if (rebuilds.isEmpty) {
          drive(g)
        } else {
          rebuilds
        }
    }
}

trait UpperAllBinaryGensOrDriveOnBinaryWhistle[C] extends PFPTransformer[C] with MutualGens[C] with BinaryWhistle[C] {
  override def rebuildings(whistle: Option[Warning], g: G): List[S] =
    whistle match {
      case None => List()
      case Some(upper) =>
        val rollbacks = mutualGens(upper.conf, g.current.conf) map translate map { rollback(upper, _) }
        if (rollbacks.isEmpty) {
          drive(g)
        } else {
          rollbacks
        }
    }
}

trait UpperMsgOrLowerMggOnBinaryWhistle[C]
  extends PFPTransformer[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(upperConf, currentConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            List(rollback(upper, conf1))
          case None =>
            val cands = rawRebuildings(currentConf) filterNot trivialRb(currentConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(rebuild(_): S).toList
        }
      case None =>
        List()
    }
  }
}

trait LowerMsgOrUpperMggOnBinaryWhistle[C] extends PFPTransformer[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace = rebuild(conf1): S
            List(replace)
          case None =>
            val cands = rawRebuildings(upperConf) filterNot trivialRb(upperConf)
            val mgg = cands find { case (c1, _) => cands forall { case (c2, _) => subclass.lteq(c2, c1) } }
            mgg.map(translate).map(rollback(upper, _)).toList
        }
      case None =>
        List()
    }
  }
}

trait MSGCurrentOrDriving[C] extends PFPTransformer[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val currentConf = g.current.conf
        val upperConf = upper.conf
        msg(currentConf, upperConf) match {
          case Some(rb) =>
            val conf1 = translate(rb)
            val replace: S = rebuild(conf1)
            List(replace)
          case None =>
            drive(g)
        }
      case None =>
        List()
    }
  }
}

trait DoubleMsgOnBinaryWhistle[C] extends PFPTransformer[C] with MSG[C] with BinaryWhistle[C] {

  def rebuildings(whistle: Option[Warning], g: G): List[S] = {
    whistle match {
      case Some(upper) =>
        val current = g.current
        val rollbacks = msg(upper.conf, current.conf) map { rb => rollback(upper, translate(rb)) }
        val rebuildings = msg(current.conf, upper.conf) map { rb => rebuild(translate(rb)): S }
        rollbacks.toList ++ rebuildings.toList
      case None =>
        List()
    }
  }
}
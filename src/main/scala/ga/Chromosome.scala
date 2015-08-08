package ga

import org.jgap
import org.jgap._


object BaseCommandGenes {
  val West  = "p"
  val East  = "b"
  val SouthWest  = "a"
  val SouthEast  = "l"
  val RotateClockwise  = "d"
  val RotateCounterClockwise  = "k"

  val BasicCommands = List(West, East, SouthWest, SouthEast, RotateClockwise, RotateCounterClockwise)
}

object CommandGene {
  val AlleleSeparator = "\t"
}


class CommandGene(conf:Configuration, alleles:List[String]) extends BaseCommandGene(conf) {

  private var allele:String = alleles(conf.getRandomGenerator.nextInt(alleles.size))

  override def getInternalValue: AnyRef = allele

  override def newGeneInternal(): jgap.Gene = new CommandGene(getConfiguration, alleles)

  override def setAllele(newAllele: scala.Any): Unit = {
    allele = newAllele.asInstanceOf[String]
  }

  override def getAllele() = allele

  override def setToRandomValue(randomGenerator: RandomGenerator): Unit = {
    alleles(randomGenerator.nextInt(alleles.size))
  }

  override def getPersistentRepresentation: String = allele + CommandGene.AlleleSeparator

  override def setValueFromPersistentRepresentation(s: String): Unit = {
    val potentialAllele = s.trim
    if (alleles contains potentialAllele) {
      allele = potentialAllele;
    } else {
      throw new UnsupportedRepresentationException(s"Unkown representation: $potentialAllele")
    }
  }

  override def applyMutation(i: Int, v: Double): Unit = {
    setAllele(alleles(getConfiguration().getRandomGenerator().nextInt(alleles.size)));
  }

  override def toString() = allele

  override def doCompare(other: Object): Int = {
    if(other == null) {
      return 1
    }

    val otherCommandGene = other.asInstanceOf[CommandGene]
    if (allele == null) {
      if(otherCommandGene.allele == null) {
        return 0
      } else {
        return -1
      }
    }
    if (otherCommandGene.allele == null) {
      return 1
    }

    return allele.compareTo(otherCommandGene.allele)
  }

  override def doEquals( otherCommandGene: Object):Boolean = {
    return otherCommandGene.isInstanceOf[CommandGene] &&
      this.doCompare( otherCommandGene ) == 0;
  }

  override def doHashCode:Int = {
    if (allele == null) {
      return 0
    }
    return allele.hashCode;
  }

}



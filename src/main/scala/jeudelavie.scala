import scala.annotation.tailrec

object jeudelavie {

  type Grille = List[(Int, Int)]

  val liste = List(
    "XX ",
    " XX",
    " XX",
    "XXX"
  )

  def main(args: Array[String]): Unit = {

    println(chainesToGrille(liste))

  }

  // 2 Entrées/sorties

  def chainesToGrille(l: List[String]): Grille = {
    @tailrec
    def aux(acc: Grille, ordonnee: Int, list: List[String]): Grille = list match {
      case t :: q => aux(determinerCoordonnees(t, ordonnee) ++ acc, ordonnee + 1, q)
      case Nil => acc
    }

    aux(List.empty, 0, l)
  }

  def determinerCoordonnees(s: String, ordonnee: Int): List[(Int, Int)] = {
    @tailrec
    def aux(acc: List[(Int, Int)], s: List[Char], abscisses: Int): List[(Int, Int)] = s match {
      case t :: q if t == 'X' => aux((ordonnee, abscisses) :: acc, q, abscisses + 1)
      case t :: q if t == ' ' => aux(acc, q, abscisses + 1)
      case Nil => acc
    }

    aux(List.empty, s.toList, 0)
  }

  def afficherGrille(g: Grille): Unit = {
    case List(i1: Int, i2: Int) => ???
  }

  @tailrec
  def pointInferieurDroit(g: Grille, gPerma: Grille): (Int, Int) = g match {
    case t::q => pointInferieurDroit(q, gPerma)
    case t::q if t._1 == t._2 => if (t._1 == g.length / dimensionGrille(gPerma)) (t._1, t._2) else pointInferieurDroit(q, gPerma)
    case Nil => (0, 0)
  }

  def dimensionGrille(g: Grille): Int = {
    def aux(g: Grille, acc: Int) = {
      5
    }
    aux(g, 0)
  }

  def pointSuperieurGauche(g: Grille): (Int, Int) = g match {
    case t::q => ???
    case Nil => ???
  }
  }


  // 3 Moteur de la simulation
  def voisines8(l: Int, c: Int): List[(Int, Int)] = {
    if (l == 0) {
      if (c == 0) {
        List()
      } else {
        List()
      }
    } else {
      List()
    }
  }

  def survivantes(g:Grille):Grille = {
    ???
  }

  def candidates(g:Grille):Grille = {
    ???
  }

  def naissances(g:Grille):Grille = {
    ???
  }

  def jeuDeLaVie(init:Grille, n:Int):Unit = {
    ???
  }
}

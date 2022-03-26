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

    afficherGrille(chainesToGrille(liste))

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
    val pInfDroit = pointInferieurDroit(g)
    val pSupGauche = pointSuperieurGauche(g)

    @tailrec
    def aux(ligne: Int, colonne: Int): Unit = {
      if (g.contains((ligne, colonne))) {
        print('X')
        aux(ligne, colonne + 1)
      } else {
        if (ligne < pInfDroit._1) {
          if (colonne < pInfDroit._2) {
            print(' ')
            aux(ligne, colonne + 1)
          } else {
            println()
            aux(ligne + 1, pSupGauche._2)
          }
        }
      }
    }
    aux(pSupGauche._1, pSupGauche._2)
  }

  def pointInferieurDroit(g: Grille): (Int, Int) = {
    g reduce ((courrant, suivant) => {
      (if (courrant._1 > suivant._1) {
        courrant._1
      } else suivant._1,
        if (courrant._2 > suivant._2) {
          courrant._2
        } else suivant._2)
    })
  }

  def pointSuperieurGauche(g: Grille): (Int, Int) = {
    g reduce ((courrant, suivant) => {
      (if (courrant._1 < suivant._1) {
        courrant._1
      } else suivant._1,
        if (courrant._2 < suivant._2) {
          courrant._2
        } else suivant._2)
    })
  }

  // 3 Moteur de la simulation

  def voisines8(l:Int, c:Int):List[(Int, Int)] = {
    (l, c - 1)::(l - 1, c - 1)::(l - 1, c)::(l - 1, c + 1)::(l, c + 1)::(l + 1, c + 1)::(l + 1, c)::(l + 1, c - 1)::Nil
  }
}
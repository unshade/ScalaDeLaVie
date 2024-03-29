import scala.annotation.tailrec

object Steiner {

  type Grille = List[(Int, Int)]
  val liste = List(
    " XX",
    "  X",
    "XXX")


  def main(args: Array[String]): Unit = {

    // test jeuDeLaVie
    moteurVariante(5)

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
        if (ligne <= pInfDroit._1) {
          if (colonne <= pInfDroit._2) {
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

  @tailrec
  def doublonRemove(g: Grille, acc: Grille): Grille = g match {
    case t :: q if acc.contains(t._1, t._2) => doublonRemove(q, acc)
    case t :: q => doublonRemove(q, t :: acc)
    case Nil => acc
  }

  def voisines8(l: Int, c: Int): List[(Int, Int)] = {
    (l, c - 1) :: (l - 1, c - 1) :: (l - 1, c) :: (l - 1, c + 1) :: (l, c + 1) :: (l + 1, c + 1) :: (l + 1, c) :: (l + 1, c - 1) :: Nil
  }

  def survivantes(g: Grille): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q if aux2(voisines8(t._1, t._2)) == 2 || aux2(voisines8(t._1, t._2)) == 3 => aux1(q, acc ++ List(t))
      case _ :: q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l: List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    doublonRemove(aux1(g, List.empty), Nil)
  }

  def candidates(g: Grille): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q =>
        val voisines = voisines8(t._1,t._2) filter(element => !g.contains(element))
        aux1(q, acc++voisines)
      case Nil => acc
    }

    doublonRemove(aux1(g, List.empty), List.empty)
  }

  def naissances(g: Grille): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q if(aux2(voisines8(t._1,t._2)) == 3) => aux1(q, acc++List(t))
      case t :: q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l: List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    doublonRemove(aux1(candidates(g), List.empty), List.empty)
  }

  @tailrec
  def jeuDeLaVie(init: Grille, n: Int): Unit = {
    if (n > 0) {
      afficherGrille(init)
      println("********************************************************")
      jeuDeLaVie(survivantes(init) ++ naissances(init),n-1)
    }
  }

  // Partie 4

  def voisines4(l: Int, c: Int): List[(Int, Int)] = {
    (l, c - 1) :: (l - 1, c) :: (l, c + 1) :: (l + 1, c) :: Nil
  }

  def naitJDLV(n: Int): Boolean = {
    n == 3
  }

  def survitJDLV(n: Int): Boolean = {
    n == 2 || n == 3
  }

  def naitF(n: Int): Boolean = {
    n % 2 == 1
  }

  def survitF(n: Int): Boolean = {
    n % 2 == 1
  }

  def survivantesG(g: Grille, r: Int => Boolean, v: (Int, Int) => List[(Int, Int)]): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q if r(aux2(v(t._1, t._2))) => aux1(q, acc ++ List(t))
      case _ :: q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l: List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    doublonRemove(aux1(g, List.empty), Nil)
  }

  def candidatesG(g: Grille, v: (Int, Int) => List[(Int, Int)]): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q =>
        val voisines = v(t._1,t._2) filter(element => !g.contains(element))
        aux1(q, acc++voisines)
      case Nil => acc
    }

    doublonRemove(aux1(g, List.empty), List.empty)
  }

  def naissancesG(g: Grille, r: Int => Boolean, v: (Int, Int) => List[(Int, Int)]): Grille = {
    @tailrec
    def aux1(grille: Grille, acc: Grille): Grille = grille match {
      case t :: q if r(aux2(v(t._1,t._2))) => aux1(q, acc++List(t))
      case t :: q => aux1(q, acc)
      case Nil => acc
    }

    def aux2(l: List[(Int, Int)]): Int = {
      l.intersect(g).length
    }

    doublonRemove(aux1(candidatesG(g,v), List.empty), List.empty)
  }

  @tailrec
  def moteur(rS: Int => Boolean, rV: Int => Boolean, v: (Int, Int) => List[(Int, Int)], init: Grille, n: Int): Unit = {
    if (n > 0) {
      afficherGrille(init)
      println("********************************************************")
      moteur(rS, rV, v, survivantesG(init, rS, v) ++ naissancesG(init, rV, v), n - 1)
    }
  }

  def moteurJeuDeLaVie(n: Int) {
    moteur(survitJDLV, naitJDLV, voisines8, chainesToGrille(liste), n)
  }

  def moteurFredkins(n: Int): Unit = {
    moteur(survitF, naitF, voisines4, chainesToGrille(liste), n)
  }

  def voisineVariante(l: Int, c: Int): List[(Int,Int)] = {
    (l + 1, c + 1)::(l - 1, c + 1)::(l - 1, c - 1)::(l + 1, c - 1)::Nil
  }

  def moteurVariante(n: Int): Unit = {
    moteur(survitF, naitF, voisineVariante, chainesToGrille(liste), n)
  }
}
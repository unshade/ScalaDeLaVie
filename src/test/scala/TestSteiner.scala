import Steiner.{liste, naitF, voisines8}
import org.scalatest._

class TestSteiner extends FunSuite {

  // tous les tests sont a partir de cette liste
  val liste = List(
    " XX",
    "  X",
    "XXX")


  test("chaineToGrille") {
    assert(Steiner.chainesToGrille(liste) == List((2,2), (2,1), (2,0), (1,2), (0,2), (0,1)))
  }

  test("voisines8") {
    assert(Steiner.voisines8(0, 0) == List((0,-1), (-1,-1), (-1,0), (-1,1), (0,1), (1,1), (1,0), (1,-1)))
  }

  test("survivantesJDLV") {
    assert(Steiner.survivantes(Steiner.chainesToGrille(liste)) == List((0,1), (0,2), (2,1), (2,2)))
  }

  test("candidatesJDLV") {
    assert(Steiner.candidates(Steiner.chainesToGrille(liste)) == List((-1,0), (0,0), (-1,3), (-1,2), (-1,1), (0,3), (3,-1), (1,-1), (2,-1), (3,0), (1,0), (3,1), (3,2), (3,3), (2,3), (1,3), (1,1)))
  }

  test("naissancesJDLV") {
    assert(Steiner.naissances(Steiner.chainesToGrille(liste)) == List((1,3), (3,1), (1,0)))
  }

  test("voisines4") {
    assert(Steiner.voisines4(0, 0) == List((0,-1), (-1,0), (0,1), (1,0)))
  }

  test("naitJDLV") {
    assert(Steiner.naitJDLV(3))
  }

  test("survitJDLV") {
    assert(Steiner.survitJDLV(2))
  }

  test("naitF") {
    assert(Steiner.naitF(7))
  }

  test("survitF") {
    assert(Steiner.survitF(5))
  }

   test("survivantesFredkin") {
    assert(Steiner.survivantesG(Steiner.chainesToGrille(liste),Steiner.survitF,Steiner.voisines4) == List((0,1), (2,0)))
  }

    test("candidatesFredkin") {
      assert(Steiner.candidatesG(Steiner.chainesToGrille(liste),Steiner.voisines4) == List((-1,1), (0,0), (0,3), (-1,2), (1,3), (3,0), (1,0), (2,-1), (3,1), (1,1), (3,2), (2,3)))
    }

    test("naissancesFredkin") {
      assert(Steiner.naissancesG(Steiner.chainesToGrille(liste),naitF,Steiner.voisines4) == List((2,3), (3,2), (1,1), (3,1), (2,-1), (1,0), (3,0), (1,3), (-1,2), (0,3), (0,0), (-1,1)))
    }


}

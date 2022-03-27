import org.scalatest._

class TestSteiner extends FunSuite {

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

  test("survivantes") {

  }

  test("candidates") {

  }

  test("naissances") {

  }


}

class Game(chances: List[Int] = Nil):
  def recordThrow(pins: Int): Game =
    Game(pins :: chances)

  def score: Int =
    score(chances.reverse, 0)

  @annotation.tailrec
  final def score(xs: List[Int], acc: Int): Int =
    xs match
      case x1 :: x2 :: x3 :: Nil =>
        acc + x1 + x2 + x3

      case 10 :: (ys@ (y1 :: y2 :: _)) =>
        score(ys, acc + 10 + y1 + y2)
      
      case x1 :: x2 :: (ys@ (y1 :: _)) if x1 + x2 == 10 =>
        score(ys, acc + x1 + x2 + y1)
      
      case x1 :: x2 :: ys =>
        score(ys, acc + x1 + x2)

      case Nil =>
        acc

      case _ :: Nil =>
        throw Exception()

def make(xs: List[Int]): Game =
  xs.foldLeft(Game()){ case (acc, x) => acc.recordThrow(x) }

@main def test(): Unit =

  val g1 = make(List(10, 5, 5) ++ List.fill(16)(0))
  assertEquals(g1, 30)

  val g2 = make(List.fill(16)(0) ++ List(5, 5, 10, 10, 10))
  assertEquals(g2, 50)

  println("OK")

def assertEquals(g: Game, expected: Int): Unit =
  val actual = g.score
  if actual != expected then
    throw Exception(s"$actual != $expected")
  else ()

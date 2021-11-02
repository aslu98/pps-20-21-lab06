package u06lab.solution

import u06lab.solution.TicTacToe._

object TicTacToe {

  sealed trait Player {
    def other: Player = this match {
      case X => O;
      case _ => X
    }

    override def toString: String = this match {
      case X => "X";
      case _ => "O"
    }
  }

  case object X extends Player

  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)

  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = Option.when(board.exists(m => m.x == x && m.y == y))(board.last.player)
  
  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for {x <- 0 to 2; y <- 0 to 2
         if find(board, x, y).isEmpty} yield board.appended(Mark(x, y, player))
  }

  def computeAnyGame(player: Player, moves: Int, stopWins: Boolean): LazyList[Game] = moves match {
    case 1 => placeAnyMark(List(), player).map(b => List(b)).to(LazyList)
    case _ => for {
      game <- computeAnyGame(player.other, moves - 1, stopWins)
      board <- placeAnyMark(game.head, player)
      if stopWins && someoneWon(board).isEmpty
    } yield board :: game
  }

  def someoneWon(board: Board): Option[Player] = {
    var player: Option[Player] = None
    for (p <- List(X, O)) {
      for (x <- 0 to 2) {
        if (List(Mark(x, 0, p), Mark(x, 1, p), Mark(x, 2, p)).forall(board.contains) ||
          List(Mark(0, x, p), Mark(1, x, p), Mark(2, x, p)).forall(board.contains)) {
          player = Some(p)
        }
      }
      if (List(Mark(0, 0, p), Mark(1, 1, p), Mark(2, 2, p)).forall(board.contains) ||
        List(Mark(0, 2, p), Mark(1, 1, p), Mark(2, 0, p)).forall(board.contains)) {
        player = Some(p)
      }
    }
    player
  }

  def printBoards(game: Seq[Board]): Unit = {
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) {
        print(" "); if (board == game.head) println()
      }
    }
  }
}

object PlayTicTacToe extends App {
  // Exercise 1: implement find such that..
  println(find(List(Mark(0,0,X)),0,0)) // Some(X)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),0,1)) // Some(O)
  println(find(List(Mark(0,0,X),Mark(0,1,O),Mark(0,2,X)),1,1)) // None

  // Exercise 2: implement placeAnyMark such that..
  printBoards(placeAnyMark(List(),X))
  // -> Seq(Mark(0,0,X), Mark(0,1,X), Mark(0,2,X),...)
  //... ... ..X ... ... .X. ... ... X..
  //... ..X ... ... .X. ... ... X.. ...
  //..X ... ... .X. ... ... X.. ... ...
  printBoards(placeAnyMark(List(Mark(0,0,O)),X))
  //O.. O.. O.X O.. O.. OX. O.. O..
  //... ..X ... ... .X. ... ... X..
  //..X ... ... .X. ... ... X.. ...

  // Exercise 3 (ADVANCED!): implement computeAnyGame such that..
  computeAnyGame(O, 4, false).zipWithIndex foreach {g => println("sol " + g._2); printBoards(g._1); println()}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
  computeAnyGame(O, 5, true).zipWithIndex foreach {g => println("sol " + g._2); printBoards(g._1); println()}
}

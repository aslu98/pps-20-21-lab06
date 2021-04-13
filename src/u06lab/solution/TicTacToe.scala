package u06lab.solution

import u06lab.solution.TicTacToe.{Mark, O, X, computeAnyGame, find, placeAnyMark, printBoards}

object TicTacToe {
  sealed trait Player{
    def other: Player = this match {case X => O; case _ => X}
    override def toString: String = this match {case X => "X"; case _ => "O"}
  }
  case object X extends Player
  case object O extends Player

  case class Mark(x: Int, y: Int, player: Player)
  type Board = List[Mark]
  type Game = List[Board]

  def find(board: Board, x: Int, y: Int): Option[Player] = board.filter(m => m.x==x && m.y == y) match {
    case Nil => None
    case board => Some(board.last.player)
  }
  def placeAnyMark(board: Board, player: Player): Seq[Board] = {
    for {x <- 0 to 2; y <- 0 to 2
         if find(board, x, y).isEmpty} yield board.appended(Mark(x, y, player))
  }

  def computeAnyGame(player: Player, moves: Int): LazyList[Game] = moves match {
    case 1 => placeAnyMark(List(),player).toList.flatMap(b => List(List(b))).to(LazyList)
    case _ => for {
      game <- computeAnyGame(player.other, moves - 1)
      board <- placeAnyMark(game.head, player)
    } yield board :: game
  }


  def printBoards(game: Seq[Board]): Unit = {
    for (y <- 0 to 2; board <- game.reverse; x <- 0 to 2) {
      print(find(board, x, y) map (_.toString) getOrElse ("."))
      if (x == 2) { print(" "); if (board == game.head) println()}
    }
  }

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
  //(computeAnyGame(O, 4).zipWithIndex) foreach {g => printBoards(g._1); println(g._2)}
  //... X.. X.. X.. XO.
  //... ... O.. O.. O..
  //... ... ... X.. X..
  //              ... computes many such games (they should be 9*8*7*6 ~ 3000).. also, e.g.:
  //
  //... ... .O. XO. XOO
  //... ... ... ... ...
  //... .X. .X. .X. .X.

  // Exercise 4 (VERY ADVANCED!) -- modify the above one so as to stop each game when someone won!!
}

object Test extends App {
  computeAnyGame(O, 4).zipWithIndex foreach {g => println("sol " + g._2); printBoards(g._1); println()}
}

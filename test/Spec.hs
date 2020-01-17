import           Control.Monad
import           Test.HUnit
import           TicTacToe

testEmptyBoard = TestCase (assertEqual "for empty board" (MkBoard [[Empty | x <- [0..2]] | y <- [0..2]]) emptyBoard)

testAddToken = TestCase (do
    let result = MkBoard [[if (x, y) == (0, 0) then X else Empty | x <- [0..2]] | y <- [0..2]]
    let b1 = addToken emptyBoard X (0, 0)
    assertEqual "for empty board after X was added in position (0, 0)" (Just result) b1
    let b2 = addToken result O (0, 0)
    assertEqual "for board after something was added twice in position (0, 0)" Nothing b2
    )

testWinner = TestCase (do
    assertEqual "for empty board" NotFinished (winner emptyBoard)
    let b1 = MkBoard [[X,X,X],[X,X,O],[O,O,X]]
    assertEqual "for board where X won horizontally" (Won Px) (winner b1)
    let b2 = MkBoard [[X,Empty,Empty],[O,O,O],[O,O,X]]
    assertEqual "for board where O won horizontally" (Won Po) (winner b2)
    let b3 = MkBoard [[if y == 1 then X else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where X won vertically" (Won Px) (winner b3)
    let b4 = MkBoard [[if y == 2 then O else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where O won vertically" (Won Po) (winner b4)
    let b5 = MkBoard [[if x == y then X else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where X won diagonally" (Won Px) (winner b5)
    let b6 = MkBoard [[if x == y then O else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where O won diagonally" (Won Po) (winner b6)
    let b7 = MkBoard [[if x == 2 - y then X else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where X won diagonally (reverse)" (Won Px) (winner b7)
    let b8 = MkBoard [[if x == 2 - y then O else Empty | x <- [0..2]] | y <- [0..2]]
    assertEqual "for board where O won diagonally (reverse)" (Won Po) (winner b8)
    let b9 = MkBoard [[O,X,O],[X,X,O],[O,O,X]]
    assertEqual "for board where draw" Draw (winner b9)
    let b10 = MkBoard [[Empty,X,O],[X,X,O],[O,O,X]]
    assertEqual "for board where nobody won yet" NotFinished (winner b10)
    )

testBestMove = TestCase (do
    let b1 = MkBoard [[Empty, Empty, Empty], [Empty, Empty, Empty], [Empty, Empty, Empty]]
    let r1X = MkBoard [[Empty, Empty, Empty], [Empty, X, Empty], [Empty, Empty, Empty]]
    let r1O = MkBoard [[Empty, Empty, Empty], [Empty, O, Empty], [Empty, Empty, Empty]]
    assertEqual "for empty board and X" r1X (bestMove b1 X)
    assertEqual "for empty board and O" r1O (bestMove b1 O)
    let b2 = MkBoard [[O, O, Empty], [Empty, Empty, X], [Empty, Empty, X]]
    let r2X = MkBoard [[O, O, X], [Empty, Empty, X], [Empty, Empty, X]]
    let r2O = MkBoard [[O, O, O], [Empty, Empty, X], [Empty, Empty, X]]
    assertEqual "for almost win board and X" r2X (bestMove b2 X)
    assertEqual "for almost win board and O" r2O (bestMove b2 O)
    let b3X = MkBoard [[O, O, Empty], [X, Empty, Empty], [Empty, X, Empty]]
    let r3X = MkBoard [[O, O, X], [X, Empty, Empty], [Empty, X, Empty]]
    let b3O = MkBoard [[O, Empty, Empty], [X, X, Empty], [Empty, O, Empty]]
    let r3O = MkBoard [[O, Empty, Empty], [X, X, O], [Empty, O, Empty]]
    assertEqual "for almost lose board and X" r3X (bestMove b3X X)
    assertEqual "for almost lose board and O" r3O (bestMove b3O O)
    let b4 = MkBoard [[Empty, Empty, Empty], [Empty, X, Empty], [Empty, Empty, Empty]]
    let r4 = MkBoard [[O, Empty, Empty], [Empty, X, Empty], [Empty, Empty, Empty]]
    assertEqual "for center is not empty" r4 (bestMove b4 O)
    let b5 = MkBoard [[Empty, Empty, Empty], [Empty, O, Empty], [Empty, Empty, Empty]]
    let r5 = MkBoard [[X, Empty, Empty], [Empty, O, Empty], [Empty, Empty, Empty]]
    assertEqual "for center is not empty" r5 (bestMove b5 X)
    )

tests = TestList [ TestLabel "emptyBoard value" testEmptyBoard
                 , TestLabel "addToken" testAddToken
                 , TestLabel "winner" testWinner
                 , TestLabel "bestmove" testBestMove
                 ]

main :: IO ()
main = void $ runTestTT tests

module Main where

import           TicTacToe

main :: IO ()
main = do
    putStrLn "Want to play with me?"
    answer <- getLine
    if (not . null) answer && head answer == 'Y'
      then do putStrLn "What you want to use?"
              xoro <- getLine
              if (not . null) xoro && head xoro == 'O'
                then do putStrLn "You are using O!"
                        go2 O emptyBoard X
                else do putStrLn "You are using X!"
                        go2 X emptyBoard X
      else go1 emptyBoard X
    where go1 b t = do print b
                       case winner b of
                           Won p -> putStrLn $ "Player " ++ show p ++ " won!"
                           Draw -> putStrLn "Draw!"
                           _ -> do putStrLn $ "Player " ++ show (player t) ++ ", your move:"
                                   (x:y:_) <- map read . words . (++ " 10 10") <$> getLine
                                   let mb' = addToken b t (x, y)
                                   case mb' of
                                       Just b' -> go1 b' (nextToken t)
                                       _ -> do putStrLn "Repeat, please:"
                                               go1 b t
          go2 pt b t = do print b
                          case winner b of
                              Won p -> putStrLn $ if pt /= t
                                                    then "You won! It's just impossible!"
                                                    else "I won!"
                              Draw -> putStrLn "Draw!"
                              _ | t == pt -> do putStrLn "Your move:"
                                                (x:y:_) <- map read . words . (++ " 10 10") <$> getLine
                                                let mb' = addToken b t (x, y)
                                                case mb' of
                                                    Just b' -> go2 pt b' (nextToken t)
                                                    _ -> do putStrLn "Repeat, please:"
                                                            go2 pt b t
                                | otherwise -> do putStrLn "My move"
                                                  let b' = bestMove b t
                                                  go2 pt (bestMove b t) pt

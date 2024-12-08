import System.IO (isEOF)
import Control.Monad (unless)
import Data.List (find)

main = do inp <- readInput []
          let mat = [ [ hasAntiNode x y inp hasAntennaPair | x <- [0..length inp - 1] ] | y <- [0..length inp - 1] ]
          print (length $ filter (\x -> x) $ concat mat)
          let mat2 = [ [ hasAntiNode x y inp hasAntennaPairP2 | x <- [0..length inp - 1] ] | y <- [0..length inp - 1] ]
          print (length $ filter (\x -> x) $ concat mat2)

readInput acc = do
                done <- isEOF
                if done then return acc
                else do inp <- getLine
                        readInput (acc ++ [inp])

hasAntiNode x y board hasAP = do 
                              let ap = hasAP x y board
                              let offsets = concat $ [ [ (x2, y2) | x2 <- [-x..(length board) - x] ] | y2 <- [-y..(length board) - y] ]
                              exists ap offsets

exists pred list = do
                   let res = find pred list
                   case res of
                      Just _ -> True
                      Nothing -> False

hasAntennaPair x y board (xStep, yStep) = do
                                          let xInBounds = (x + xStep * 2) < (length board) && (x + xStep * 2) >= 0
                                          let yInBounds = (y + yStep * 2) < (length board) && (y + yStep * 2) >= 0
                                          let inBounds = xInBounds && yInBounds && (xStep /= 0 || yStep /= 0)
                                          if inBounds then do
                                            let next = getFreqAt (x + xStep) (y + yStep) board
                                            let notDot = next /= '.'
                                            let match = next == (getFreqAt (x + (xStep * 2)) (y + (yStep * 2)) board)
                                            (notDot && match)
                                          else False

hasAntennaPairP2 x y board (xStep, yStep) = do
                                            let xInBounds = (x + xStep) < (length board) && (x + xStep) >= 0
                                            let yInBounds = (y + yStep) < (length board) && (y + yStep) >= 0
                                            let inBounds = xInBounds && yInBounds && (xStep /= 0 || yStep /= 0)
                                            if inBounds then do
                                              let cur = getFreqAt x y board
                                              let next = getFreqAt (x + xStep) (y + yStep) board
                                              let notDot = next /= '.'
                                              let match = next == cur
                                              (notDot && match) || hasAntennaPairP2 (x + xStep) (y + yStep) board (xStep, yStep)
                                            else False

getFreqAt x y board = (board !! y) !! x

import System.IO (isEOF)
import Control.Monad (unless)

main = inputLoop

inputLoop = do done <- isEOF
               unless done $
                      do inp <- getLine
                         putStrLn (inp)
                         inputLoop

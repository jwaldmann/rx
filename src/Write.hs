module Write where

class Monad m => Write m where
      writeStr :: String -> m ()

      writeStrLn :: String -> m ()
      writeStrLn cs = writeStr ( cs ++ "\n" )

      writeChar :: Char -> m ()
      writeChar c = writeStr [c]

      get_contents :: m String
      read_file :: FilePath -> m String

instance Write IO where
      writeStr = putStr
      writeStrLn = putStrLn
      writeChar = putChar
      get_contents = getContents
      read_file = readFile



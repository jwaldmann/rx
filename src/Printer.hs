-- | used for HTML output in CGI

{-# language TypeSynonymInstances #-}
{-# language FlexibleInstances #-}

module Printer where

import Write
import Control.Monad.State

data Conf = Conf { output :: [ String ] -- ^ in reverse order
		 , input  :: String
		 }

type Printer = State Conf

instance Write Printer where

      writeStr cs = do
          c <- get
	  put $ c { output = cs : output c }

      get_contents = do
          c <- get
	  return $ input c

      read_file f = do
          error "read_file not implemented for CGI"

start :: String -> Conf
start cs = Conf { output = []
	        , input = cs 
		}

exec :: String -> Printer () -> String
exec cs p = evalState 
    ( do p ; c <- get ; return $ concat $ reverse $ output c )
    ( start cs )



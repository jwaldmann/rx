-- | Main module for CGI script

--   $Id$

import Network.CGI
import Text.XHtml 

-- import Control.Concurrent
-- import Control.Exception

import qualified GMain

import Printer

patience :: Int
patience = 30 -- seconds

main :: IO ()
main = do
    wrapper $ \ env -> do
{-
            forkIO $ do
                threadDelay $ patience * 10 ^ 6
	        error "timer expired"
-}
            let Just rxi = lookup "RX_INPUT" env
            let rxo = exec rxi ( GMain.gmain "-" )
	    return $ toHtml rxo -- p << pre << primHtml ( rxo      )
        `catch` \ err -> 
	    return $ toHtml $ show err -- p << pre << primHtml ( show err )


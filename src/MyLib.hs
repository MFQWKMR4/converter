module MyLib where
import System.Environment (getArgs)
import GHC.Base (returnIO)

router :: IO ()
router = do
    args <- getArgs

    case head args of
        "base58check" -> putStrLn "not implemented yet ..."
        _ -> putStrLn "not implemented yet"




module MyLib where
import System.Environment (getArgs)
import GHC.Base (returnIO)
import Bits

wapper :: IO ()
wapper = do
    args <- getArgs
    router args

router :: [String] ->IO ()
router s =
    case head s of
        "base58check" -> putStrLn "not implemented yet ..."
        "aaa" -> print (myBin2dec "110")
        _ -> putStrLn "not implemented yet"

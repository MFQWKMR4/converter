module Main(main) where


import Test.Hspec
import Bits

main :: IO ()
main = hspec $ do
    tests

tests :: Spec
tests = do
    describe "standard" $ do
        it "myDec2bin" $
            myDec2bin 10 `shouldBe` "1010"
        it "myDec2bin" $
            myDec2bin 11 `shouldBe` "1011"
        it "zeroPadding" $
            zeroPadding 8 "1011" `shouldBe` "00001011"
        it "zeroPadding" $
            zeroPadding 8 "00001011" `shouldBe` "00001011"
        it "littleendian2int" $
            littleendian2int "0000000100000010" `shouldBe` 257
        it "littleendian2int 2" $
            littleendian2int "10000000000000000000000000000000" `shouldBe` 128
        it "int2littleendian 2" $
            int2littleendian 4 128 `shouldBe` "10000000000000000000000000000000"
        it "int2bigendian 2" $
            int2bigendian 4 128 `shouldBe` "00000000000000000000000010000000"

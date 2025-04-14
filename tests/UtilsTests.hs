{-
-- EPITECH PROJECT, 2023
-- Utilstests.hs
-- File description:
-- Utilstests
-}

module UtilsTests where

import Test.Hspec
import Utils

test_setAt :: Spec
test_setAt = do
  describe "setAt" $ do
    it "should set a value at the beginning of the list" $ do
      let lst = [1, 2, 3, 4, 5]
      let updatedList = setAt 0 10 lst
      updatedList `shouldBe` [10, 2, 3, 4, 5]

    it "should set a value in the middle of the list" $ do
      let lst = [1, 2, 3, 4, 5]
      let updatedList = setAt 2 10 lst
      updatedList `shouldBe` [1, 2, 10, 4, 5]

    it "should set a value at the end of the list" $ do
      let lst = [1, 2, 3, 4, 5]
      let updatedList = setAt 4 10 lst
      updatedList `shouldBe` [1, 2, 3, 4, 10]

    it "should handle an out-of-bounds index by not modifying the list" $ do
      let lst = [1, 2, 3, 4, 5]
      let updatedList = setAt 10 10 lst
      updatedList `shouldBe` [1, 2, 3, 4, 5]

    it "should handle an empty list by not modifying the list" $ do
      let lst = [] :: [Int]
      let updatedList = setAt 0 10 lst
      updatedList `shouldBe` []

    it "should handle setting a value at the last index" $ do
      let lst = [1, 2, 3, 4, 5]
      let updatedList = setAt 4 10 lst
      updatedList `shouldBe` [1, 2, 3, 4, 10]


test_charsToInt :: Spec
test_charsToInt = do
  describe "charsToInt" $ do
    it "should convert four null characters to 0" $ do
      let chars = ['\0', '\0', '\0', '\0']
      let result = charsToInt chars
      result `shouldBe` 0

    it "should convert a list of characters to a positive integer" $ do
      let chars = ['\NUL', '\SOH', '\STX', '\ETX'] -- Equivalent to [0, 1, 2, 3]
      let result = charsToInt chars
      result `shouldBe` 66051

    it "should convert a list of characters to a negative integer" $ do
      let chars = ['\DEL', '\255', '\255', '\255'] -- Equivalent to [-1, -1, -1, -1]
      let result = charsToInt chars
      result `shouldBe` (-1)

    it "should handle input with more than four characters by ignoring extra characters" $ do
      let chars = ['\0', '\0', '\0', '\0', 'X', 'Y']
      let result = charsToInt chars
      result `shouldBe` 0

    it "should handle input with less than four characters by treating missing characters as null" $ do
      let chars = ['\0', '\0']
      let result = charsToInt chars
      result `shouldBe` 0


test_charsToUInt :: Spec
test_charsToUInt = do
  describe "charsToUInt" $ do
    it "should convert four null characters to 0" $ do
      let chars = ['\0', '\0', '\0', '\0']
      let result = charsToUInt chars
      result `shouldBe` 0

    it "should convert a list of characters to an unsigned integer" $ do
      let chars = ['\NUL', '\SOH', '\STX', '\ETX'] -- Equivalent to [0, 1, 2, 3]
      let result = charsToUInt chars
      result `shouldBe` 66051

    it "should handle input with more than four characters by ignoring extra characters" $ do
      let chars = ['\0', '\0', '\0', '\0', 'X', 'Y']
      let result = charsToUInt chars
      result `shouldBe` 0

    it "should handle input with less than four characters by treating missing characters as null" $ do
      let chars = ['\0', '\0']
      let result = charsToUInt chars
      result `shouldBe` 0


test_uintToChars :: Spec
test_uintToChars = do
    describe "uintToChars" $ do
        it "should convert 0 to four null characters" $ do
          let int = 0
          let result = uintToChars int
          result `shouldBe` [0, 0, 0, 0]
      
        it "should convert a positive integer to a list of characters" $ do
          let int = 66051
          let result = uintToChars int
          result `shouldBe` [0, 1, 2, 3]
      
        it "should handle a negative integer by converting it to a positive integer" $ do
          let int = (-1)
          let result = uintToChars int
          result `shouldBe` [255, 255, 255, 255]

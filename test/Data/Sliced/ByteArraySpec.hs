{-# OPTIONS_GHC -F -pgmF solid-pp #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE RecordWildCards #-}
module Data.Sliced.ByteArraySpec (arbitrary, spec) where

import Helper hiding (pack, unpack, shouldThrow, take, drop, reverse)
import Test.Hspec (shouldThrow)
use Gen
use Range

use Data.Char
import Control.Arrow ((&&&))
import Data.Semigroup
import GHC.Exts (fromList)

import Data.Sliced.ByteArray as ByteArray
import Data.Sliced.ByteArray.Unsafe as ByteArray
import Data.Sliced.ByteArray.Util

import Hedgehog.Classes
import Hedgehog.Internal.Property

ord8 :: Char -> Word8
ord8 = fromIntegral . Char.ord

chr8 :: Word8 -> Char
chr8 = Char.chr . fromIntegral

isAlphaNum :: Word8 -> Bool
isAlphaNum = Char.isAlphaNum . chr8

word8 :: MonadGen m => m Word8
word8 = Gen.word8 Range.constantBounded

bytes :: MonadGen m => Range Int -> m ByteArray
bytes = bytesWith word8

smallBytes :: MonadGen m => Range Int -> m ByteArray
smallBytes = bytesWith $ Gen.word8 (Range.constant 0 5)

bytesWith :: MonadGen m => m Word8 -> Range Int -> m ByteArray
bytesWith gen range = do
  size <- Gen.int range
  off <- Gen.int (Range.constant 0 size)
  end <- Gen.int (Range.constant off size)
  let len = end - off
  arr <- fromList <$> Gen.list (Range.singleton size) gen
  return ByteArray{..}

arbitrary :: MonadGen m => m ByteArray
arbitrary = bytes (Range.linear 0 10)

newtype Predicate a = Predicate (a -> Bool)

instance Show (Predicate a) where
  show _ = "<predicate>"

predicate :: MonadGen m => m (Predicate Word8)
predicate = do
  codomain <- Gen.list (Range.singleton 256) Gen.bool
  return . Predicate $ \ n -> codomain !! (fromIntegral n)

satisfies :: HasCallStack => Gen a -> (Gen a -> Laws) -> Spec
satisfies gen laws = do
  describe (className <> " instance") $ do
    forM_ properties $ \ (name, p) -> do
      it ("satisfies " <> name) (propertyTest p)
  where
    Laws className properties = laws gen

instance ToString ByteArray where

instance ToString Array where

instance HasField "toString" ByteArray String where
  getField = toString

instance HasField "toString" Array String where
  getField = toString

prop :: HasCallStack => [Char] -> PropertyT IO () -> Spec
prop s = it s . hedgehog

spec :: Spec
spec = do
  arbitrary `satisfies` eqLaws
  arbitrary `satisfies` showLaws
  arbitrary `satisfies` semigroupLaws
  arbitrary `satisfies` monoidLaws

  describe "stimes" $ do
    it "is defined on 0" $ do
      stimes @ByteArray (0 :: Integer) "foo" `shouldBe` mempty

    context "when n is negative" $ do
      it "returns mempty" $ do
        let n = pred $ toInteger (minBound :: Int)
        stimes @ByteArray n " " `shouldBe` mempty

    context "when n overflows Int" $ do
      let n = succ $ toInteger (maxBound :: Int)

      context "with mempty" $ do
        it "returns mempty" $ do
          stimes @ByteArray n mempty `shouldBe` mempty

      context "with a subject of non-zero length" $ do
        it "throws an exception" $ do
          evaluate (stimes @ByteArray n " ") `shouldThrow` errorCall "Data.Sliced.ByteArray.stimes: size overflow"

  describe "show" $ do
    context "with valid UTF-8" $ do
      it "shows a textual representation" $ do
        let input = "foo" :: ByteArray
        show input `shouldBe` "\"foo\""

    context "with invalid UTF-8" $ do
      it "shows a list of bytes" $ do
        let input = "fo" <> pack [0xf6]
        show input `shouldBe` "[0x66, 0x6f, 0xf6]"

    it "correctly handles length and offset" $ do
      input <- forAll arbitrary
      show input === show (ByteArray.copy input)

  describe "pack" $ do
    it "packs a list of bytes into a ByteArray" $ do
      ByteArray.pack [102, 111, 111, 98, 97, 114] `shouldBe` "foobar"

    it "is inverse to unpack" $ do
      input <- forAll arbitrary
      pack (unpack input) === input

  describe "unpack" $ do
    it "unpacks a list of bytes from a ByteArray" $ do
      unpack "foobar" `shouldBe` [102, 111, 111, 98, 97, 114]

  describe "singleton" $ do
    it "creates a singleton" $ do
      c <- forAll word8
      ByteArray.singleton c === pack [c]

  describe "empty" $ do
    it "has a length of zero" $ do
      ByteArray.empty.len `shouldBe` 0

  describe "cons" $ do
    it "prepends a byte" $ do
      ByteArray.cons (ord8 'f') "oo" `shouldBe` "foo"

    it "is inverse to uncons" $ do
      input <- forAll arbitrary
      maybe mempty (uncurry ByteArray.cons) (ByteArray.uncons input) === input

  describe "snoc" $ do
    it "appends a byte" $ do
      ByteArray.snoc "ba" (ord8 'r') `shouldBe` "bar"

    it "is inverse to unsnoc" $ do
      input <- forAll arbitrary
      maybe mempty (uncurry ByteArray.snoc) (ByteArray.unsnoc input) === input

  describe "append" $ do
    it "appends two byte arrays" $ do
      append "foo" "bar" `shouldBe` "foobar"

  describe "uncons" $ do
    it "is inverse to cons" $ do
      x <- forAll word8
      xs <- forAll arbitrary
      ByteArray.uncons (ByteArray.cons x xs) === Just (x, xs)

  describe "unsnoc" $ do
    it "is inverse to snoc" $ do
      x <- forAll word8
      xs <- forAll arbitrary
      ByteArray.unsnoc (ByteArray.snoc xs x) === Just (xs, x)

  describe "head" $ do
    it "extracts the first element" $ do
      ByteArray.head "foo" `shouldBe` ord8 'f'

    it "throws on empty" $ do
      evaluate (ByteArray.head "") `shouldThrow` errorCall "empty ByteArray"

  describe "last" $ do
    it "extracts the last element" $ do
      ByteArray.last "bar" `shouldBe` ord8 'r'

    it "throws on empty" $ do
      evaluate (ByteArray.last "") `shouldThrow` errorCall "empty ByteArray"

  describe "tail" $ do
    it "drops the first element" $ do
      ByteArray.tail "bar" `shouldBe` "ar"

    it "throws on empty" $ do
      evaluate (ByteArray.tail "") `shouldThrow` errorCall "empty ByteArray"

  describe "init" $ do
    it "drops the last element" $ do
      ByteArray.init "bar" `shouldBe` "ba"

    it "throws on empty" $ do
      evaluate (ByteArray.init "") `shouldThrow` errorCall "empty ByteArray"

  describe "null" $ do
    context "with an empty ByteArray" $ do
      it "returns True" $ do
        ByteArray.null "" `shouldBe` True

    context "with a non-empty ByteArray" $ do
      it "returns False" $ do
        ByteArray.null "foo" `shouldBe` False

  describe "length" $ do
    it "returns the length of a ByteArray" $ do
      ByteArray.length "foo" `shouldBe` 3

  describe "map" $ do
    it "applies a function to every byte" $ do
      let f = (+ 1)
      input <- forAll arbitrary
      ByteArray.map f input === (pack . List.map f . unpack) input

  describe "reverse" $ do
    it "reverses a ByteArray" $ do
      ByteArray.reverse "foobar" `shouldBe` "raboof"

    it "is inverse to itself" $ do
      input <- forAll arbitrary
      ByteArray.reverse (ByteArray.reverse input) === input

  describe "intersperse" $ do
    it "intersperses a byte between the elements of a ByteArray" $ do
      ByteArray.intersperse (ord8 '-') "foobar" `shouldBe` "f-o-o-b-a-r"

    it "works with arbitrary input" $ do
      x <- forAll word8
      xs <- forAll arbitrary
      ByteArray.intersperse x xs === (pack . List.intersperse x . unpack) xs

  describe "intercalate" $ do
    it "intersperses a ByteArray between the elements of a list of byte arrays" $ do
      ByteArray.intercalate " - " ["foo", "bar", "baz"] `shouldBe` "foo - bar - baz"

    it "works with arbitrary input" $ do
      x <- forAll arbitrary
      xs <- forAll $ Gen.list (Range.constant 0 10) arbitrary
      ByteArray.intercalate x xs === mconcat (List.intersperse x xs)

  describe "foldl" $ do
    it "folds from left to right" $ do
      input <- forAll arbitrary
      ByteArray.foldl (flip (:)) [] input === List.reverse (unpack input)

    it "is non-strict" $ do
      _ <- evaluate $ ByteArray.foldl (flip (:)) undefined "foo"
      pass

  describe "foldl'" $ do
    it "folds from left to right" $ do
      input <- forAll arbitrary
      ByteArray.foldl' (flip (:)) [] input === List.reverse (unpack input)

    it "is strict" $ do
      evaluate (ByteArray.foldl' (flip (:)) undefined "foo") `shouldThrow` errorCall "Prelude.undefined"

  describe "foldl1" $ do
    it "folds from left to right" $ do
      ByteArray.foldl1 (-) [1..10] `shouldBe` 203

  describe "foldl1'" $ do
    it "folds from left to right" $ do
      ByteArray.foldl1' (-) [1..10] `shouldBe` 203

  describe "foldr" $ do
    it "folds from right to left" $ do
      input <- forAll arbitrary
      ByteArray.foldr (:) [] input === unpack input

    it "is non-strict" $ do
      _ <- evaluate $ ByteArray.foldr (:) undefined "foo"
      pass

  describe "foldr'" $ do
    it "folds from right to left" $ do
      input <- forAll arbitrary
      ByteArray.foldr' (:) [] input === unpack input

    it "is strict" $ do
      evaluate (ByteArray.foldr' (:) undefined "foo") `shouldThrow` errorCall "Prelude.undefined"

  describe "foldr1" $ do
    it "folds from right to left" $ do
      ByteArray.foldr1 (-) [1..10] `shouldBe` 251

  describe "foldr1'" $ do
    it "folds from right to left" $ do
      ByteArray.foldr1' (-) [1..10] `shouldBe` 251

  describe "concat" $ do
    it "concatenates a list of byte arrays" $ do
      ByteArray.concat ["foo", "bar", "baz"] `shouldBe` "foobarbaz"

  describe "any" $ do
    it "tests whether any element of a byte array satisfies a predicate" $ do
      input <- forAll arbitrary
      ByteArray.any isAlphaNum input === List.any isAlphaNum (unpack input)

  describe "all" $ do
    it "tests whether all elements of a byte array satisfy a predicate" $ do
      input <- forAll arbitrary
      ByteArray.all isAlphaNum input === List.all isAlphaNum (unpack input)

  describe "isAscii" $ do
    it "tests whether a byte array contains only ASCII code-points" $ do
      input <- forAll arbitrary
      ByteArray.isAscii input === List.all (< 128) (unpack input)

  describe "isValidUtf8" $ do
    context "with valid UTF-8" $ do
      it "returns True" $ do
        input <- forAll $ Gen.list (Range.constant 0 10) Gen.unicodeScalar
        isValidUtf8 (fromString input) === True

    context "with invalid UTF-8" $ do
      it "returns False" $ do
        let input = ByteArray [0, 128] 1 1
        isValidUtf8 input `shouldBe` False

  describe "take" $ do
    context "with a non-negative number" $ do
      it "takes from the beginning of the list" $ do
        ByteArray.take 3 "foobarbaz" `shouldBe` "foo"

    context "with a negative number" $ do
      it "takes from the end of the list" $ do
        ByteArray.take -3 "foobarbaz" `shouldBe` "baz"

  describe "drop" $ do
    context "with a non-negative number" $ do
      it "drops from the beginning of the list" $ do
        ByteArray.drop 3 "foobarbaz" `shouldBe` "barbaz"

    context "with a negative number" $ do
      it "drops from the end of the list" $ do
        ByteArray.drop -3 "foobarbaz" `shouldBe` "foobar"

  describe "splitAt" $ do
    it "is reversed by (<>)" $ do
      input <- forAll arbitrary
      n <- forAll $ Gen.int (Range.constant -20 20)
      uncurry (<>) (ByteArray.splitAt n input) === input

    context "with a non-negative number" $ do
      it "splits, counting from the beginning of the list" $ do
        ByteArray.splitAt 3 "foobarbaz" === ("foo", "barbaz")
        input <- forAll arbitrary
        n <- forAll $ Gen.int (Range.constant 0 20)
        ByteArray.splitAt n input === (ByteArray.take n &&& ByteArray.drop n) input

    context "with a negative number" $ do
      it "splits, counting from the end of the list" $ do
        ByteArray.splitAt -3 "foobarbaz" === ("foobar", "baz")
        input <- forAll arbitrary
        n <- forAll $ Gen.int (Range.constant -20 -1)
        ByteArray.splitAt n input === (ByteArray.drop n &&& ByteArray.take n) input

  describe "takeWhile" $ do
    it "takes while a predicate holds" $ do
      ByteArray.takeWhile (< 5) [1..10] `shouldBe` [1..4]

    context "with a predicate that always holds" $ do
      it "is the identity" $ do
        input <- forAll arbitrary
        ByteArray.takeWhile (const True) input === input

  describe "dropWhile" $ do
    it "drops while a predicate holds" $ do
      ByteArray.dropWhile (< 5) [1..10] `shouldBe` [5..10]

  describe "span" $ do
    it "takes / drops while a predicate holds" $ do
      ByteArray.span (< 5) [1..10] === ([1..4], [5..10])
      input <- forAll arbitrary
      Predicate p <- forAll predicate
      ByteArray.span p input === (ByteArray.takeWhile p &&& ByteArray.dropWhile p) input

    it "is reversed by (<>)" $ do
      input <- forAll arbitrary
      Predicate p <- forAll predicate
      uncurry (<>) (ByteArray.span p input) === input

  describe "break" $ do
    it "takes / drops while a predicate does not hold" $ do
      ByteArray.break (> 4) [1..10] `shouldBe` ([1..4], [5..10])

  describe "takeWhileEnd" $ do
    it "takes from the end while a predicate holds" $ do
      ByteArray.takeWhileEnd (> 5) [1..10] `shouldBe` [6..10]

    context "with a predicate that always holds" $ do
      it "is the identity" $ do
        input <- forAll arbitrary
        ByteArray.takeWhileEnd (const True) input === input

  describe "dropWhileEnd" $ do
    it "drops from the end while a predicate holds" $ do
      ByteArray.dropWhileEnd (> 5) [1..10] `shouldBe` [1..5]

  describe "spanEnd" $ do
    it "takes / drops from the end while a predicate holds" $ do
      ByteArray.spanEnd (> 5) [1..10] === ([1..5], [6..10])
      input <- forAll arbitrary
      Predicate p <- forAll predicate
      ByteArray.spanEnd p input === (ByteArray.dropWhileEnd p &&& ByteArray.takeWhileEnd p) input

    it "is reversed by (<>)" $ do
      input <- forAll arbitrary
      Predicate p <- forAll predicate
      uncurry (<>) (ByteArray.spanEnd p input) === input

  describe "breakEnd" $ do
    it "takes / drops from the end while a predicate does not hold" $ do
      ByteArray.breakEnd (< 6) [1..10] `shouldBe` ([1..5], [6..10])

  describe "stripPrefix" $ do
    it "strips a prefix" $ do
      ByteArray.stripPrefix "foo" "foobarbaz" `shouldBe` (Just "barbaz")

  describe "stripSuffix" $ do
    it "strips a suffix" $ do
      ByteArray.stripSuffix "baz" "foobarbaz" `shouldBe` (Just "foobar")

  describe "isPrefixOf" $ do
    it "tests whether a byte array starts with a given prefix" $ do
      isPrefixOf "foo" "foobarbaz" `shouldBe` True

    prop "isPrefixOf (take n input) input == True" $ do
      input <- forAll arbitrary
      n <- forAll $ Gen.int (Range.constant 0 input.len)
      isPrefixOf (take n input) input === True

    prop "isPrefixOf prefix input == isSuffixOf (reverse prefix) (reverse input)" $ do
      prefix <- forAll $ smallBytes (Range.linear 0 10)
      input <- forAll $ smallBytes (Range.linear 0 100)
      isPrefixOf prefix input === isSuffixOf (reverse prefix) (reverse input)

  describe "isSuffixOf" $ do
    it "tests whether a byte array ends with a given suffix" $ do
      isSuffixOf "baz" "foobarbaz" `shouldBe` True

    prop "isSuffixOf (take -n input) input == True" $ do
      input <- forAll arbitrary
      n <- forAll $ Gen.int (Range.constant 0 input.len)
      isSuffixOf (take -n input) input === True

    prop "isSuffixOf suffix input == isPrefixOf (reverse suffix) (reverse input)" $ do
      suffix <- forAll $ smallBytes (Range.linear 0 10)
      input <- forAll $ smallBytes (Range.linear 0 100)
      isSuffixOf suffix input === isPrefixOf (reverse suffix) (reverse input)

  describe "times" $ do
    it "throws an exception on overflow" $ do
      evaluate (ByteArray.times maxBound "foo") `shouldThrow` errorCall "Data.Sliced.ByteArray.times: size overflow"

{-# LANGUAGE FlexibleContexts #-}
module LearnHaskDA.Appendix where
import Text.Regex.Posix
test161 :: RegexContext Regex String target => target
test161  = "My name is Jim." =~ "Jim"
test161a = test161 :: Bool
test161b = ("My name is Frank." =~ "Jim") :: Bool

test161aMathces = test161 :: Int
test161aStartLen = test161 :: (Int, Int)

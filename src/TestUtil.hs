{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module TestUtil where

import Language.Haskell.TH.Syntax

--assert :: (Lift a, Eq a, Show a) => a -> a -> Q Exp
--assert actual expected = inlineAssert' actual expected

assert :: (Lift a, Eq a, Show a) => a -> a -> Q Exp
assert actual expected = do
  if actual /= expected then
    fail $
      "Actual:   " ++ show actual ++ "\n" ++ "        " ++
      "Expected: " ++ show expected ++ "\n"
  else lift actual

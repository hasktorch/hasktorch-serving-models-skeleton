{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Test () where

import Lib
import TestUtil
import Torch

test :: Float
test = $(assert (asValue (bboxIou (asTensor [0 :: Float,0,9,9]) (asTensor [5 :: Float,5,9,9]))) (0.25::Float))

test2 :: [Int]
test2 = $(assert (shape (asTensor [0 :: Float,0,10,10])) [3])

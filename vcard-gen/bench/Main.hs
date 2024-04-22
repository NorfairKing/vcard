{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import VCard
import VCard.Gen ()

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @VCard
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @VCard
        ]
    ]

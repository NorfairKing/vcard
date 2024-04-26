{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import VCard
import VCard.Component.Gen ()
import VCard.Component.V4 as V4

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @VCard,
          genValidBench @V4.Card
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @VCard,
          shrinkValidBench @V4.Card
        ]
    ]

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Criterion.Main as Criterion
import Data.GenValidity.Criterion
import Data.GenValidity.Vector ()
import VCard
import VCard.Component.Gen ()
import VCard.V3 as V3
import VCard.V4 as V4

main :: IO ()
main = do
  Criterion.defaultMain
    [ bgroup
        "generators"
        [ genValidBench @VCard,
          genValidBench @V4.Card,
          genValidBench @V3.Card
        ],
      bgroup
        "shrinkers"
        [ shrinkValidBench @VCard,
          shrinkValidBench @V4.Card,
          shrinkValidBench @V3.Card
        ]
    ]

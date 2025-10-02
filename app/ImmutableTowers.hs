-- |
-- Module      : ImmutableTowers
-- Description : Apenas contém a forma de dados do jogo...
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
module ImmutableTowers where

import LI12425 (Jogo)

data ImmutableTowers = ImmutableTowers
  { estadoJogo :: Jogo,
    windowSize :: (Int, Int),
    tileSize :: Float
  }
  deriving (Show)

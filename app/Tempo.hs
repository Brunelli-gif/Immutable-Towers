-- |
-- Module      : Tempo
-- Description : Atualiza o jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
module Tempo where

import ImmutableTowers
import LI12425
import Tarefa2
import Tarefa3

reageTempo :: Tempo -> ImmutableTowers -> ImmutableTowers
reageTempo dt it
  | not $ pausa (estadoJogo it) && not (terminouJogo (estadoJogo it)) = it {estadoJogo = atualizaJogo dt (estadoJogo it)}
  | otherwise = it

-- |
-- Module      : Eventos
-- Description : O centro de operações dos controlos do jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
module Eventos where

import GHC.Float
import Graphics.Gloss.Interface.Pure.Game
import ImmutableTowers
import LI12425
import Tarefa1

reageEventos :: Event -> ImmutableTowers -> ImmutableTowers
reageEventos (EventResize (x, y)) it = it {windowSize = (max 600 x, max 400 y)}
reageEventos (EventKey (SpecialKey KeySpace) Down _ _) it = it {estadoJogo = colocaTorre (estadoJogo it)}
reageEventos (EventKey (Char 'w') Down _ _) it = it {estadoJogo = moveCursor (estadoJogo it) Norte}
reageEventos (EventKey (Char 's') Down _ _) it = it {estadoJogo = moveCursor (estadoJogo it) Sul}
reageEventos (EventKey (Char 'a') Down _ _) it = it {estadoJogo = moveCursor (estadoJogo it) Oeste}
reageEventos (EventKey (Char 'd') Down _ _) it = it {estadoJogo = moveCursor (estadoJogo it) Este}
reageEventos (EventKey (Char '1') Down _ _) it = it {estadoJogo = (estadoJogo it) {selectedIndex = 0}}
reageEventos (EventKey (Char '2') Down _ _) it = it {estadoJogo = (estadoJogo it) {selectedIndex = 1}}
reageEventos (EventKey (Char '3') Down _ _) it = it {estadoJogo = (estadoJogo it) {selectedIndex = 2}}
reageEventos (EventKey (Char 'p') Down _ _) it = it {estadoJogo = (estadoJogo it) {pausa = not (pausa (estadoJogo it))}}
reageEventos _ it = it

moveCursor :: Jogo -> Direcao -> Jogo
moveCursor jogo dir
  | dir == Norte = jogo {cursorPosition = (cx, max 0 (cy - 1))}
  | dir == Sul = jogo {cursorPosition = (cx, min maxH (cy + 1))}
  | dir == Este = jogo {cursorPosition = (min maxW (cx + 1), cy)}
  | dir == Oeste = jogo {cursorPosition = (max 0 (cx - 1), cy)}
  | dir == None = jogo
  where
    (cx, cy) = cursorPosition jogo

    mapa = mapaJogo jogo

    (maxW, maxH) = (length (head mapa), length mapa)
moveCursor jogo _ = jogo -- não deve chegar aqui, mas o lsp gosta de se queixar

colocaTorre :: Jogo -> Jogo
colocaTorre jogo = jogo'
  where
    mapa = mapaJogo jogo

    loja = lojaJogo jogo

    base = baseJogo jogo

    (cx, cy) = cursorPosition jogo

    posicoesTorres = map posicaoTorre (torresJogo jogo)

    (custo, torreSelecionada) = loja !! selectedIndex jogo

    torrePorAdicionar = torreSelecionada {posicaoTorre = (int2Float cx, int2Float cy)}

    jogo' =
      if terrenoPos mapa (int2Float cx, int2Float cy) == Relva && creditosBase base >= custo && notElem (int2Float cx, int2Float cy) posicoesTorres
        then jogo {torresJogo = torrePorAdicionar : torresJogo jogo, baseJogo = base {creditosBase = creditosBase base - custo}}
        else jogo

-- |
-- Module      : Tarefa1
-- Description : Invariantes do Jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 1 de LI1 em 2024/25.
module Tarefa1 where

import Data.Bits
import Data.Maybe (isJust, listToMaybe, mapMaybe)
import GHC.Float
import LI12425

-- | Valida o estado do `Jogo`
validaJogo :: Jogo -> Bool
validaJogo j = and condicoes
  where
    base = baseJogo j
    portais = portaisJogo j
    torres = torresJogo j
    mapa = mapaJogo j
    inimigos = inimigosJogo j

    posBase = posicaoBase base

    condicoes =
      [ not (null portais), -- Existe pelo menos um portal
        all
          ( \p ->
              posBase /= posicaoPortal p -- Nenhum portal coincide com a base
                && portalEmTerra mapa p -- Todos os portais estão colocados em Terra
                && validaCaminhoPortalBase mapa posBase (posicaoPortal p) -- Existe um caminho de cada portal para a base
                && posNCoincideComTorres (posicaoPortal p) torres -- Nenhum portal coincide com nenhuma das torres
                && nOndasAtivas (ondasPortal p) <= 1 -- Apenas uma onda ativa por portal, no máximo
                && all (\o -> validaInimigosInativos (inimigosOnda o) (posicaoPortal p)) (ondasInativas $ ondasPortal p)
          )
          portais,
        all
          ( \i ->
              terrenoPos mapa (posicaoInimigo i) == Terra
                && posNCoincideComTorres (posicaoInimigo i) torres
                && velocidadeInimigo i >= 0
                && projeteisInimigoNormalizados (projeteisInimigo i)
          )
          inimigos,
        all
          ( \t ->
              terrenoPos mapa (posicaoTorre t) == Terra
                && alcanceTorre t > 0
                && rajadaTorre t > 0
                && cicloTorre t >= 0
                -- \| Verifica que não há duas torres com a mesma posição
                && length (filter (== posicaoTorre t) (posicoesTorres torres)) == 1
          )
          torres,
        terrenoPos mapa posBase == Terra,
        creditosBase base >= 0,
        posNCoincideComPortais posBase portais,
        posNCoincideComTorres posBase torres
      ]

-- | Retorna todas as `Posicao` das `Torre` do `Jogo`
posicoesTorres :: [Torre] -> [Posicao]
posicoesTorres = map posicaoTorre

-- | Verifica se os projeteis aplicados a um inimigo estão normalizados.
-- Ou seja, seguem as regras do jogo.
projeteisInimigoNormalizados :: [Projetil] -> Bool
projeteisInimigoNormalizados projeteis = not temDuplicados && naoFogoGelo && naoFogoResina
  where
    tpa = tiposProjeteisAplicados projeteis
    temDuplicados = any (`elem` tail tpa) tpa

    naoFogoGelo = (Fogo `elem` tpa) `xor` (Gelo `elem` tpa)
    naoFogoResina = (Fogo `elem` tpa) `xor` (Resina `elem` tpa)

-- | Mapeia todos os `Projetil` aplicados a um `Inimigo` para os seus respetivos `TipoProjetil`
tiposProjeteisAplicados :: [Projetil] -> [TipoProjetil]
tiposProjeteisAplicados = map tipoProjetil

-- | Valida os `Inimigo`s inativos para que nenhum se encontre fora da sua torre,
-- com vida negativa ou com projéteis sobre si.
validaInimigosInativos :: [Inimigo] -> Posicao -> Bool
validaInimigosInativos [] _ = True
validaInimigosInativos (i : is) pt = pt == pInimigo && vida > 0 && null projInim && validaInimigosInativos is pt
  where
    pInimigo = posicaoInimigo i
    vida = vidaInimigo i
    projInim = projeteisInimigo i

-- | Conta as `Onda`s ativas de um `Portal`
nOndasAtivas :: [Onda] -> Int
nOndasAtivas [] = 0
nOndasAtivas (o : os)
  | entradaOnda o <= 0 = 1 + nOndasAtivas os
  | otherwise = nOndasAtivas os

-- | Retorna a lista das ondas inativas
ondasInativas :: [Onda] -> [Onda]
ondasInativas = filter (\o -> entradaOnda o > 0)

-- | Verifica se um `Portal` está em Terra
portalEmTerra :: Mapa -> Portal -> Bool
portalEmTerra mapa p = terrenoPos mapa (x, y) == Terra
  where
    (x, y) = posicaoPortal p

-- | Verifica que, dada uma `Posicao`, não coincide com as `Posicao` das `Torre`s no `Jogo`
posNCoincideComTorres :: Posicao -> [Torre] -> Bool
posNCoincideComTorres _ [] = True
posNCoincideComTorres pos (t : ts) = pos /= pTorre && posNCoincideComTorres pos ts
  where
    pTorre = posicaoTorre t

-- | Similar a `posNCoincideComTorres` mas para `Portal`
posNCoincideComPortais :: Posicao -> [Portal] -> Bool
posNCoincideComPortais _ [] = True
posNCoincideComPortais pos (p : ps) = pos /= pPortal && posNCoincideComPortais pos ps
  where
    pPortal = posicaoPortal p

-- | Very self explanatory...
--
-- == Exemplo
--
-- @
--   mapa :: Mapa
--   --        v--- Portal
--   mapa = [ [Terra, Terra, Agua, Terra],
--            [Terra, Terra, Agua, Terra],
--            [Agua, Terra, Terra, Terra],
--            [Terra, Terra, Terra, Terra]
--   --                             ^---- Base
--          ]
--
--   validaCaminhoPortalBase mapa (0, 0) (3, 3) == True
-- @
validaCaminhoPortalBase :: Mapa -> Posicao -> Posicao -> Bool
validaCaminhoPortalBase mapa start posbase = isJust $ dfs mapa start posbase []

-- | Função de busca em profundidade (DFS - Depth-First Search) que retorna o caminho.
--
-- Retorna Just caminho se encontrar um caminho válido,
-- Nothing caso contrário.
dfs :: Mapa -> Posicao -> Posicao -> [Posicao] -> Maybe [Posicao]
dfs mapa current@(cx, cy) goal@(gx, gy) visited
  | currentFloored == goalFloored = Just (reverse (currentFlooredFloat : visited)) -- Encontrou o caminho, inverte a lista
  | current `elem` visited = Nothing -- Evitar ciclos
  | otherwise = listToMaybe $ mapMaybe (\neighbor -> dfs mapa neighbor goal (current : visited)) validNeighbors
  where
    neighbors = adjacents currentFloored
    validNeighbors = filter (isValidNeighbor mapa visited) neighbors

    currentFloored :: (Int, Int)
    currentFloored = (floor cx, floor cy)

    currentFlooredFloat :: Posicao
    currentFlooredFloat = (int2Float $ floor cx, int2Float $ floor cy)

    goalFloored :: (Int, Int)
    goalFloored = (floor gx, floor gy)

-- | Lista de vizinhos adjacentes
adjacents :: (Int, Int) -> [Posicao]
adjacents (x, y) = [(x' - 1, y'), (x' + 1, y'), (x', y' - 1), (x', y' + 1)]
  where
    x' = int2Float x
    y' = int2Float y

-- | Verifica se um vizinho é válido para explorar
isValidNeighbor :: Mapa -> [Posicao] -> Posicao -> Bool
isValidNeighbor mapa visited pos =
  notElem pos visited -- Ainda não visitado
    && terrenoValido pos -- O terreno é válido
  where
    terrenoValido p = withinBounds p && terrenoPos mapa p == Terra
    withinBounds (x', y') = x >= 0 && y >= 0 && x < length mapa && y < length (head mapa)
      where
        x = float2Int x'
        y = float2Int y'

-- | Busca o `Terreno` no `Mapa` dada uma `Posicao`
terrenoPos :: Mapa -> Posicao -> Terreno
terrenoPos mapa (x, y)
  | x < 0 || float2Int x > length mapa = error "X out of bounds"
  | y < 0 || float2Int y > length (head mapa) = error "Y out of bounds"
  | otherwise = (mapa !! float2Int y) !! float2Int x

-- | Converte o caminho encontrado em [Direcao] que o inimigo segue
pathToDirections :: [Posicao] -> [Direcao]
pathToDirections [] = []
pathToDirections [_] = []
pathToDirections ((x1, y1) : p2@(x2, y2) : ps)
  | x1 == x2 && y2 > y1 = Sul : pathToDirections (p2 : ps)
  | x1 == x2 && y2 < y1 = Norte : pathToDirections (p2 : ps)
  | y1 == y2 && x2 > x1 = Este : pathToDirections (p2 : ps)
  | y1 == y2 && x2 < x1 = Oeste : pathToDirections (p2 : ps)
  | otherwise = pathToDirections (p2 : ps)

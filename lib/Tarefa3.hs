-- |
-- Module      : Tarefa3
-- Description : Mecânica do Jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 3 de LI1 em 2024/25.
module Tarefa3 where

import Data.List
import Data.Maybe (mapMaybe)
import GHC.Float
import LI12425
  ( Base (posicaoBase),
    Direcao (..),
    Duracao (Finita),
    Inimigo (..),
    Jogo (baseJogo, inimigosJogo, mapaJogo, portaisJogo, tempoJogo, torresJogo),
    Onda (..),
    Portal (..),
    Posicao,
    Projetil (duracaoProjetil, tipoProjetil),
    Tempo,
    TipoProjetil (Fogo, Gelo, Resina),
    Torre (..),
    creditosBase,
    vidaBase,
  )
import Tarefa1 (dfs)
import Tarefa2 (atingeInimigo, distance, inimigosNoAlcance)

-- | Atualiza o estado do jogo
--
-- WARNING: Portais por implementar
-- Warning: Inimigos q chegam a base são eliminados e dão dano, mas a implementação pode ser problemática
atualizaJogo :: Tempo -> Jogo -> Jogo
atualizaJogo dt jogo = jogo'
  where
    -- =====================
    -- I Variáveis do Jogo I
    -- =====================
    -- Por favor manter a
    -- ordem pela qual se
    -- dispoem as variáveis
    -- sincronizada com as
    -- suas correspondentes
    -- atualizadas abaixo

    inimigos = inimigosJogo jogo

    torres = torresJogo jogo

    base = baseJogo jogo

    portais = portaisJogo jogo

    -- =========================
    -- I Variáveis atualizadas I
    -- =========================

    -- inimigos com todos os efeitos aplicados
    inimigos' = inimigosAtualizados jogo dt $ foldl (flip (inimigosAtingidos 1)) inimigos torres

    torres' = atualizaTorres dt torres (inimigosAlcance inimigos' torres)

    base' = baseLevaDanoDeInimigo base {creditosBase = creditosBase base + (length inimigos - length inimigos') * 200} inimigos'

    (portais', inimigosPorLançar) = portaisInimigosPorLançar $ map (inimigoLargado dt . diminuiTempoEntradaOnda dt . pruneOndas . ordenaOndas) portais

    jogo' = jogo {inimigosJogo = inimigos' ++ inimigosPorLançar, torresJogo = torres', tempoJogo = tempoJogo jogo + dt, baseJogo = base', portaisJogo = portais'}

-- ============
-- I Inimigos I
-- ============

-- | Aplica recursivamente os efeitos de uma torre dada uma
-- lista de inimigos (supostamente) ao seu alcance
inimigosAtingidos :: Int -> Torre -> [Inimigo] -> [Inimigo]
inimigosAtingidos _ _ [] = []
inimigosAtingidos ac t (i : is)
  | tempoTorre t <= 0 && rajadaTorre t >= ac && distance (posicaoInimigo i) (posicaoTorre t) <= alcanceTorre t = atingeInimigo t i : inimigosAtingidos (ac + 1) t is
  | otherwise = i : inimigosAtingidos ac t is

-- | Atualiza o estado dos inimigos após serem atingidos
-- pelas `Torre`s.
inimigosAtualizados :: Jogo -> Float -> [Inimigo] -> [Inimigo]
inimigosAtualizados _ _ [] = []
inimigosAtualizados jogo dt (i : is)
  | vidaInimigo i > 0 = moveInimigo dt (aplicaEfeitosDuracao dt i) : inimigosAtualizados jogo dt is
  | otherwise = inimigosAtualizados jogo dt is

-- | Quase q se explica a si mesma, aplica efeitos de dano com duração,
-- por exemplo o Fogo.
aplicaEfeitosDuracao :: Float -> Inimigo -> Inimigo
aplicaEfeitosDuracao dt ini = ini {vidaInimigo = vida, projeteisInimigo = projeteisInimigo'}
  where
    tpa = map tipoProjetil (projeteisInimigo ini)

    projDurAtua :: [Projetil]
    projDurAtua = map (\p -> p {duracaoProjetil = duracaoProjetil p - Finita dt}) (projeteisInimigo ini)

    durProjetilFogo = maybe 0 duracaoProjetil (getProjetilFromTipo (projeteisInimigo ini) Fogo)

    -- \| Fogo inflige 10 dano por segundo
    fogoInfligeDano :: Float
    fogoInfligeDano = if Fogo `elem` tpa && floor durProjetilFogo - floor (durProjetilFogo - Finita dt) == (1 :: Int) then 2 else 0

    projeteisInimigo' = filter (\p -> duracaoProjetil p > 0) projDurAtua

    vida = vidaInimigo ini - fogoInfligeDano

-- | Encontra um Projetil a partir do seu TipoProjetil
getProjetilFromTipo :: [Projetil] -> TipoProjetil -> Maybe Projetil
getProjetilFromTipo [] _ = Nothing
getProjetilFromTipo (proj : projs) tipo
  | tipoProjetil proj == tipo = Just proj
  | otherwise = getProjetilFromTipo projs tipo

-- | Move o inimigo em direção à próxima posição
moveInimigo :: Float -> Inimigo -> Inimigo
moveInimigo dt i = inimigo'
  where
    inimigo' =
      i
        { direcaoInimigo = if distDirecaoAtual' > 1 then nextDir (pathInimigo i) else direcaoInimigo i,
          distDirecaoAtual = if distDirecaoAtual' > 1 then 0 else distDirecaoAtual',
          pathInimigo = if distDirecaoAtual' > 1 then tail (pathInimigo i) else pathInimigo i,
          posicaoInimigo = if distDirecaoAtual' > 1 then roundPosition $ mv (direcaoInimigo i) else mv (direcaoInimigo i)
        }

    nextDir :: [Direcao] -> Direcao
    nextDir [] = None
    nextDir [_] = None
    nextDir (_ : dir : _) = dir

    mv :: Direcao -> Posicao
    mv Norte = (px, py - distPercorrida)
    mv Sul = (px, py + distPercorrida)
    mv Este = (px + distPercorrida, py)
    mv Oeste = (px - distPercorrida, py)
    mv None = posicaoInimigo i

    (px, py) = posicaoInimigo i

    distDirecaoAtual' = distDirecaoAtual i + distPercorrida

    distPercorrida = if direcaoInimigo i /= None then velocidadeInimigo i * dt * multiplier else 0

    tpa = map tipoProjetil (projeteisInimigo i)

    -- Movement debuff multiplier
    multiplier :: Float
    multiplier
      | Gelo `elem` tpa = 0
      | Resina `elem` tpa = 0.6
      | otherwise = 1

-- | Trunca a posicao
truncatePosition :: Posicao -> Posicao
truncatePosition (x, y) = (int2Float $ truncate x, int2Float $ truncate y)

-- | Arredonda as componentes da posicao para os inteiros mais proximos
roundPosition :: Posicao -> Posicao
roundPosition (x, y) = (int2Float $ round x, int2Float $ round y)

-- | Normalize a 2D vector
normalize :: (Float, Float) -> (Float, Float)
normalize (x, y)
  | magnitude == 0 = (0, 0)
  | otherwise = (x / magnitude, y / magnitude)
  where
    magnitude = sqrt (x ** 2 + y ** 2)

-- | Encontra a próxima posição
findNext :: Jogo -> Posicao -> Maybe Posicao
findNext jogo (x, y) = nextPos path
  where
    -- coisas complicadas acontecem aqui...
    path = dfs (mapaJogo jogo) (x, y) (posicaoBase $ baseJogo jogo) []

    nextPos :: Maybe [Posicao] -> Maybe Posicao
    nextPos Nothing = Nothing
    nextPos (Just []) = Nothing
    nextPos (Just ps) = Just (head (tail ps)) -- weird way bc LSP cries about the normal way

-- ==========
-- I Torres I
-- ==========

-- | Assumindo as torres estão na mesma ordem,
-- e o resultado de não ter inimigos ao alcance
-- será uma lista vazia, isto será usado
-- para ativar a torre correspondente e disparar
-- sobre os inimigos
inimigosAlcance :: [Inimigo] -> [Torre] -> [[Inimigo]]
inimigosAlcance inimigos = map (`inimigosNoAlcance` inimigos)

-- | Atualiza os `tempoTorre` para todas as torres em jogo de acordo
-- com o seu estado, se disparou ou não
atualizaTorres :: Float -> [Torre] -> [[Inimigo]] -> [Torre]
atualizaTorres _ [] _ = []
atualizaTorres _ _ [] = error "What? Para isto ocorrer teve de não haver torres a um ponto... EM Q DEVIA TER TORRES"
atualizaTorres dt (t : ts) (_ : is)
  -- Se o tempo para a próxima rajada for 0 ou negativo e os inimigos a disparar não for vazio
  -- reseta o tempoTorre para cicloTorre.
  | tempoTorre t <= 0 = t {tempoTorre = cicloTorre t} : atualizaTorres dt ts is
  -- Como o seu tempo para a próxima rajada não é 0, atualiza o seu tempo para se aproximar de 0.
  | otherwise = t {tempoTorre = tempoTorre t - dt} : atualizaTorres dt ts is

-- ========
-- I Base I
-- ========

-- | Define a base quando leva dano de um Inimigo
baseLevaDanoDeInimigo :: Base -> [Inimigo] -> Base
baseLevaDanoDeInimigo base [] = base
baseLevaDanoDeInimigo base (i : is) = baseLevaDanoDeInimigo base' is
  where
    base' = if roundPosition (posicaoBase base) == roundPosition (posicaoInimigo i) then base {vidaBase = vidaBase base - ataqueInimigo i} else base

-- ===========
-- I Portais I
-- ===========

-- | Ordena as ondas para fácil manipulação
ordenaOndas :: Portal -> Portal
ordenaOndas p = p {ondasPortal = sort (ondasPortal p)}

-- | Atualiza o tempo de entrada das ondas de um portal
diminuiTempoEntradaOnda :: Tempo -> Portal -> Portal
diminuiTempoEntradaOnda dt p = p {ondasPortal = map (\o -> o {entradaOnda = entradaOnda o + dt}) (ondasPortal p)}

-- | Remove as ondas sem Inimigos por lançar
pruneOndas :: Portal -> Portal
pruneOndas p = p {ondasPortal = filter (not . null . inimigosOnda) (ondasPortal p)}

-- | Se um Portal encontra-se pronto para largar um inimigo em Jogo
-- a função retorna o portal atualizado e talvez um inimigo largado
inimigoLargado :: Tempo -> Portal -> (Portal, Maybe Inimigo)
inimigoLargado dt p = case ondasPortal p of
  [] -> (p, Nothing)
  (o : os) ->
    let (o', ini) = ondaLargaInimigo o
     in (p {ondasPortal = o' : os}, ini)
  where
    ondaLargaInimigo :: Onda -> (Onda, Maybe Inimigo)
    ondaLargaInimigo onda
      | null (inimigosOnda onda) = (onda, Nothing)
      | tempoOnda onda <= 0 =
          let newEnemies = tail (inimigosOnda onda)
           in (onda {tempoOnda = cicloOnda onda, inimigosOnda = newEnemies}, Just (head $ inimigosOnda onda))
      | otherwise = (onda {tempoOnda = tempoOnda onda - dt}, Nothing)

-- | Transforma o resultado mapado dos Portais através de `inimigoLargado`
-- num par de listas dos Portais em Jogo e dos inimigos por lançar nesse mesmo frame
portaisInimigosPorLançar :: [(Portal, Maybe Inimigo)] -> ([Portal], [Inimigo])
portaisInimigosPorLançar xs =
  let portals = map fst xs
      inimigos = mapMaybe snd xs
   in (portals, inimigos)

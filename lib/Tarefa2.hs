-- |
-- Module      : Tarefa2
-- Description : Auxiliares do Jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
--
--
-- Módulo para a realização da Tarefa 2 de LI1 em 2024/25.
module Tarefa2 where

import Data.List (sort)
import Data.Maybe (isJust)
import LI12425
  ( Base (vidaBase),
    Duracao,
    Inimigo (posicaoInimigo, projeteisInimigo, vidaInimigo),
    Jogo (baseJogo, inimigosJogo, portaisJogo),
    Onda (inimigosOnda),
    Portal (ondasPortal),
    Posicao,
    Projetil (duracaoProjetil, tipoProjetil),
    TipoProjetil (Fogo, Gelo, Resina),
    Torre (alcanceTorre, danoTorre, posicaoTorre, projetilTorre),
  )
import Tarefa1 (tiposProjeteisAplicados)

-- | Filtra os `Inimigo`s em `Jogo` para os que estão ao alcance de uma dada `Torre`
inimigosNoAlcance :: Torre -> [Inimigo] -> [Inimigo]
inimigosNoAlcance t = filter (\i -> distance posT (posicaoInimigo i) <= alcance)
  where
    alcance = alcanceTorre t
    posT = posicaoTorre t

-- | Dada uma `Torre` e um `Inimigo`, aplica os efeitos da torre ao inimigo.
--
-- Isto inclui o tipo de `Projetil` e as suas sinergias com os projéteis já aplicados ao inimigo.
atingeInimigo :: Torre -> Inimigo -> Inimigo
atingeInimigo t ini = ini'
  where
    vida = max (vidaInimigo ini - danoTorre t) 0

    ini' =
      ini
        { vidaInimigo = vida,
          projeteisInimigo = avaliaNovoProjetil (projetilTorre t) (projeteisInimigo ini)
        }

-- | Adiciona um novo `Projetil` ao `Inimigo`, seguindo as regras do jogo
--
-- TODO: EXPLICAR O FUNCIONAMENTO E PORQUÊ DE ALGUMAS ESCOLHAS DE DESIGN
avaliaNovoProjetil :: Projetil -> [Projetil] -> [Projetil]
avaliaNovoProjetil proj projs
  -- Sinergias Fogo/Gelo
  | tipoProjetil proj == Fogo && Gelo `elem` tiposProjeteisAplicados projs = removeTipoProjetil Gelo projs
  | tipoProjetil proj == Gelo && Fogo `elem` tiposProjeteisAplicados projs = removeTipoProjetil Fogo projs
  -- Sinergia Resina/Fogo
  | tipoProjetil proj == Resina && Fogo `elem` tiposProjeteisAplicados projs =
      map (\p -> if tipoProjetil p == Fogo then p {duracaoProjetil = duracaoProjetil p * 2} else p) projs
  -- Sinergia Fogo/Resina
  | tipoProjetil proj == Fogo && Resina `elem` tiposProjeteisAplicados projs =
      proj {duracaoProjetil = duracaoProjetil proj * 2} : removeTipoProjetil Resina projs
  | tipoProjetil proj `elem` tiposProjeteisAplicados projs = adicionaDuracaoProjetil (tipoProjetil proj) (duracaoProjetil proj) projs
  -- Não há sinergias a registar
  | otherwise = proj : projs

-- | Remove os `Projetil` de uma lista `[Projetil]` de acordo com o seu `TipoProjetil`
--
-- Aplicado para a sinergias que se anulam ou anulam outros tipos.
removeTipoProjetil :: TipoProjetil -> [Projetil] -> [Projetil]
removeTipoProjetil tipo = filter (\p -> tipoProjetil p /= tipo)

-- | Adiciona a duração do `Projetil` que atingiu um inimigo mais recentemente ao `Projetil`
-- do seu `TipoProjetil` correspondente e retorna a lista atualizado `[Projetil]`
adicionaDuracaoProjetil :: TipoProjetil -> Duracao -> [Projetil] -> [Projetil]
adicionaDuracaoProjetil tipo dur =
  map (\p -> if tipoProjetil p == tipo then p {duracaoProjetil = duracaoProjetil p + dur} else p)

-- | Função que detecta se os inimigos estão no portal, se eles estão, remove eles adicionando os mesmos
--  no cenário do jogo, tendo como função base um portal vazio, devolvendo o resultado de que não tem
--  inimigos spawnados
ativaInimigo :: Portal -> [Inimigo] -> (Portal, [Inimigo])
ativaInimigo portal ini
  | isJust $ proxIni ondasIniNNulos = (portal, i : ini)
  | otherwise = (portal, ini) -- Não há mais inimigos para mandar
  where
    ondas = ondasPortal portal
    sortedOndas = sort ondas
    ondasIniNNulos = filter (not . null . inimigosOnda) sortedOndas

    proxIni :: [Onda] -> Maybe Inimigo
    proxIni [] = Nothing
    proxIni (o : _) = Just (head $ inimigosOnda o)

    (Just i) = proxIni ondasIniNNulos

-- | Função que define se o jogador ganhou o jogo ou perdeu o jogo
terminouJogo :: Jogo -> Bool
terminouJogo jogo = ganhouJogo jogo || perdeuJogo jogo

-- | Função que verifica se tem algum inimigo vivo tanto no cenário do jogo, quanto no portal de spawn dos
--  inimigos, se não houver nenhum já spawnado ou pra spawnar, ele faz null da função, se ela retornar vazia
--  dá como resultado True, mostrando assim no ecrã que o jogador venceu, se houver algum inimigo vivo
--  ele não faz nada e o jogo continua rodando
ganhouJogo :: Jogo -> Bool
ganhouJogo jogo = null (inimigosJogo jogo) && null inimigosInativos
  where
    ps = portaisJogo jogo
    ondas = concatMap ondasPortal ps
    inimigosInativos = concatMap inimigosOnda ondas

-- | Função que verifica se há algum inimigo na base do jogador, se tiver algum inimigo la o comando da
--  gameover pro jogo, se não houver nenhum inimigo lá, não acontece nada
perdeuJogo :: Jogo -> Bool
perdeuJogo jogo = vidaBase (baseJogo jogo) <= 0

-- | Calcula a distância entre duas Posições (`Posicao`)
distance :: Posicao -> Posicao -> Float
distance (x1, y1) (x2, y2) = sqrt ((x2 - x1) ** 2 + (y2 - y1) ** 2)

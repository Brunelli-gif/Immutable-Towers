-- |
-- Module      : Desenhar
-- Description : Desenha o jogo
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
module Desenhar where

import GHC.Float
import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa2

-- =========
-- I Geral I
-- =========

-- | Tendo o mapa do jogo, o tamanho das telhas e da Window, calcula
-- o fator de zoom para que as telhas encaixem no ecrã
zoomFactor :: Mapa -> Float -> (Int, Int) -> Float
zoomFactor mapa size (wx, wy) =
  min
    (int2Float wx / (gridWidth * size))
    (int2Float wy / (gridHeight * size))
  where
    gridHeight = int2Float $ length (head mapa)
    gridWidth = int2Float $ length mapa

-- ========
-- I Mapa I
-- ========

-- | Dado uma telha de Terreno do mapa, converte numa
-- Picture que pode ser renderizada.
terrenoToPicture :: Terreno -> Float -> Posicao -> Picture
terrenoToPicture t size (x, y) = Translate x y $ Color (terrainColor t) $ rectangleSolid size size
  where
    terrainColor :: Terreno -> Color
    terrainColor Terra = orange
    terrainColor Agua = blue
    terrainColor Relva = green

-- | Converte o mapa numa lista de Pictures a ser
-- renderizadas para o ecrã
mapaToPictures :: Mapa -> Float -> [Picture]
mapaToPictures mapa tile =
  [terrenoToPicture terreno tile (x * tile, y * (-tile)) | (y, linha) <- zip [0 ..] mapa, (x, terreno) <- zip [0 ..] linha]

-- =============
-- I Entidades I
-- =============

-- | Converte a base numa Picture para ser renderizada
baseToPicture :: Base -> Float -> Picture
baseToPicture base size = Translate x y $ Color cyan $ rectangleSolid 150 150
  where
    (px, py) = posicaoBase base

    (x, y) = (px * size, py * (-size))

-- | Converte as Torres do Jogo em Pictures para serem renderizadas
torresToPicture :: [Torre] -> Float -> [Picture]
torresToPicture [] _ = []
torresToPicture (t : ts) size = Translate x y (Color (colorTipoTorre tipoTorre) $ rectangleSolid 150 150) : Translate x y (Color red $ circle (alcanceTorre t * size)) : torresToPicture ts size
  where
    (px, py) = posicaoTorre t
    tipoTorre = tipoProjetil (projetilTorre t)
    (x, y) = (px * size, py * (-size))

-- | Converte os Portais do Jogo em Pictures para serem renderizadas
portaisToPicture :: [Portal] -> Float -> [Picture]
portaisToPicture [] _ = []
portaisToPicture (p : ps) size = Translate x y (Color red $ rectangleSolid 150 150) : portaisToPicture ps size
  where
    (px, py) = posicaoPortal p
    (x, y) = (px * size, py * (-size))

-- | Converte Inimigos em Jogo em Pictures para serem renderizadas
inimigosToPicture :: [Inimigo] -> Float -> [Picture]
inimigosToPicture [] _ = []
inimigosToPicture (i : is) size = Translate x y (Color corInimigo $ rectangleSolid 50 50) : Translate x (y - 50) (Color red $ rectangleSolid (vidaInimigo i * 10) 20) : inimigosToPicture is size
  where
    (px, py) = posicaoInimigo i
    (x, y) = (px * size, py * (-size))

    tpa = map tipoProjetil (projeteisInimigo i)

    corInimigo = if not $ null tpa then mixColors 0.5 0.5 yellow (colorTipoTorre (head tpa)) else yellow

-- ========
-- I Loja I
-- ========

-- | Renderiza o UI do lado do mapa
renderSideUI :: Jogo -> Int -> (Float, Float) -> Creditos -> Picture
renderSideUI jogo selected (wx, wy) creditos = ui
  where
    loja = lojaJogo jogo
    base = baseJogo jogo

    (containerX, containerY) = (wx * 0.25, wy)

    background = Color (makeColorI 50 50 50 200) $ rectangleSolid containerX containerY

    textScale
      | containerX > 400 = 0.8
      | containerX > 300 = 0.6
      | containerX > 250 = 0.5
      | containerX > 200 = 0.4
      | containerX > 150 = 0.3
      | otherwise = 0.25

    instructionsTitle = Translate (-(containerX / 2) + 25) (wy / 2 - 100 * textScale) $ Scale (0.5 * textScale) (0.5 * textScale) $ Color white $ Text "Como jogar:"
    instructionsMove = Translate (-(containerX / 2) + 25) (wy / 2 - 200 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text "W|A|S|D - Move cursor"
    instructionsPause = Translate (-(containerX / 2) + 25) (wy / 2 - 250 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text "P - Pausa inimigos"
    instructionsSelect = Translate (-(containerX / 2) + 25) (wy / 2 - 300 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text "1..9 - Seleciona Torre"
    instructionsPlace = Translate (-(containerX / 2) + 25) (wy / 2 - 350 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text "Espaco - Coloca Torre"

    statusTitle = Translate (-(containerX / 2) + 25) (wy / 2 - 450 * textScale) $ Scale (0.5 * textScale) (0.5 * textScale) $ Color white $ Text "Status:"
    statusPause = Translate (-(containerX / 2) + 25) (wy / 2 - 500 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text (if pausa jogo then "Pausado" else "A correr")
    statusVida = Translate (-(containerX / 2) + 25) (wy / 2 - 550 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text ("Vida: " ++ show (vidaBase base))
    statusTempo = Translate (-(containerX / 2) + 25) (wy / 2 - 600 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text ("Tempo: " ++ show (truncate (tempoJogo jogo) :: Int))

    lojaTitle = Translate (-(containerX / 2) + 25) (wy / 2 - 700 * textScale) $ Scale (0.5 * textScale) (0.5 * textScale) $ Color white $ Text "Loja"

    creds = Translate (-(containerX / 2) + 25) (wy / 2 - 750 * textScale) $ Scale (0.3 * textScale) (0.3 * textScale) $ Color white $ Text ("Creditos: " ++ show creditos)

    torres =
      Translate (-(containerX / 2) + 70) (wy / 2 - 900 * textScale) $
        Pictures $
          placementUI (zipWith (\idx (c, t) -> selecaoTorre (c, t) selected idx) [0 ..] loja) (100, 100) containerX

    ui =
      Translate (-(wx / 2) + containerX / 2) 0 $
        Pictures
          [ background,
            lojaTitle,
            instructionsTitle,
            instructionsMove,
            instructionsPause,
            instructionsSelect,
            instructionsPlace,
            statusTitle,
            statusPause,
            statusVida,
            statusTempo,
            creds,
            torres
          ]

-- | Coloca elementos de forma a seguirem as regras de uma Flexbox em HTML
placementUI :: [Picture] -> (Float, Float) -> Float -> [Picture]
placementUI picts (w, h) limit = zipWith translatePicture picts positions
  where
    positions =
      take (length picts) $
        concatMap
          ( \row ->
              let rowPositions =
                    [ (x, -y)
                      | let y = fromIntegral row * (h + 10),
                        x <-
                          takeWhile
                            (<= limit - w)
                            [fromIntegral col * (w + 10) | col <- [(0 :: Int) ..]]
                    ]
               in if null rowPositions then [] else rowPositions
          )
          [(0 :: Int) ..]

    -- Translate a picture to its computed position
    translatePicture :: Picture -> (Float, Float) -> Picture
    translatePicture pic (x, y) = Translate x y pic

-- | Gera um elemento de UI com a torre e os seus dados associados da Loja
selecaoTorre :: (Creditos, Torre) -> Int -> Int -> Picture
selecaoTorre (preco, t) selected current = selecaoUI
  where
    tipoTorre = tipoProjetil (projetilTorre t)
    torre = Color (colorTipoTorre tipoTorre) $ rectangleSolid 50 50
    background = Color (if selected == current then makeColorI 200 150 100 255 else makeColorI 100 100 100 255) $ rectangleSolid 100 100

    precoText = Translate (-45) (-45) $ Scale 0.1 0.1 $ Color white $ Text (show preco)

    tipoText = Translate (-45) 35 $ Scale 0.1 0.1 $ Color white $ Text (show tipoTorre)

    selecaoUI = Pictures [background, torre, precoText, tipoText]

-- | Associa uma cor ao tipo do Projetil, usado maioritariamente para definir a cor de uma Torre
colorTipoTorre :: TipoProjetil -> Color
colorTipoTorre Fogo = makeColorI 255 100 100 255
colorTipoTorre Resina = makeColorI 255 255 100 255
colorTipoTorre Gelo = makeColorI 100 100 255 255

-- ==========
-- I Cursor I
-- ==========

-- | Mostra o cursor no mapa
cursorToPicture :: (Int, Int) -> Float -> Picture
cursorToPicture (cx, cy) size = Translate (int2Float cx * size) (int2Float (-cy) * size) $ Color (makeColor 0 0 0 0.5) $ rectangleSolid size size

-- ========
-- I Main I
-- ========

-- | Renderiza o estado do jogo
desenha :: ImmutableTowers -> Picture
desenha immTwrs
  | ganhouJogo jogo = Pictures [Color green $ rectangleSolid (int2Float wx) (int2Float wy), Translate (-450) 0 $ Scale 1.5 1.5 $ Text "Ganhou!"]
  | perdeuJogo jogo = Pictures [Color red $ rectangleSolid (int2Float wx) (int2Float wy), Translate (-450) 0 $ Scale 1.5 1.5 $ Text "Perdeu..."]
  | otherwise = Pictures [sideUIRender, Translate (int2Float (-wx) / 2 + int2Float wx * 0.25) (int2Float wy / 2) $ Scale zoom zoom $ Translate (tile / 2) (-(tile / 2)) gameRender]
  where
    (wx, wy) = windowSize immTwrs
    tile = tileSize immTwrs

    jogo = estadoJogo immTwrs

    mapa = mapaJogo jogo

    base = baseJogo jogo

    torres = torresJogo jogo

    portais = portaisJogo jogo

    inimigos = inimigosJogo jogo

    zoom = zoomFactor mapa tile (wx, wy)

    mapaRender = mapaToPictures mapa tile

    entidadesRender =
      concat
        [ [baseToPicture base tile],
          torresToPicture torres tile,
          portaisToPicture portais tile,
          inimigosToPicture inimigos tile
        ]

    cursorRender = [cursorToPicture (cursorPosition jogo) tile]

    sideUIRender = renderSideUI jogo (selectedIndex jogo) (int2Float wx, int2Float wy) (creditosBase base)

    gameRender =
      Pictures $
        concat
          [ mapaRender,
            entidadesRender,
            cursorRender
          ]

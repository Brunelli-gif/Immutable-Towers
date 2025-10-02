-- |
-- Module      : Main
-- Description : O ficheiro onde começou tudo! Muahahahahahaha
-- Copyright   : Miguel André Leal Santos <a111485@alunos.uminho.pt>
--               Matheus Salvalaggio Brunelli <a109359@alunos.uminho.pt>
module Main where

import Data.Maybe
import Desenhar
import Eventos
import Graphics.Gloss
import ImmutableTowers
import LI12425
import Tarefa1
import Tempo

o :: Terreno
o = Terra

º :: Terreno
º = Relva

i :: Terreno
i = Agua

jogo :: Jogo
jogo =
  Jogo
    { baseJogo =
        Base
          { vidaBase = 100,
            posicaoBase = (10, 10),
            creditosBase = 500
          },
      torresJogo =
        [],
      pausa = True,
      inimigosJogo = [],
      lojaJogo =
        [ ( 400,
            Torre
              { projetilTorre = Projetil {tipoProjetil = Fogo, duracaoProjetil = 3},
                cicloTorre = 1,
                tempoTorre = 0,
                posicaoTorre = (-1, -1),
                danoTorre = 4,
                alcanceTorre = 3,
                rajadaTorre = 1
              }
          ),
          ( 200,
            Torre
              { projetilTorre = Projetil {tipoProjetil = Resina, duracaoProjetil = 6},
                cicloTorre = 2,
                tempoTorre = 0,
                posicaoTorre = (-1, -1),
                danoTorre = 1,
                alcanceTorre = 4,
                rajadaTorre = 3
              }
          ),
          ( 500,
            Torre
              { projetilTorre = Projetil {tipoProjetil = Gelo, duracaoProjetil = 2},
                cicloTorre = 1,
                tempoTorre = 0,
                posicaoTorre = (-1, -1),
                danoTorre = 1,
                alcanceTorre = 2,
                rajadaTorre = 1
              }
          )
        ],
      mapaJogo =
        [ [o, o, o, º, º, º, º, º, º, º, º, i, i, i, i, i, i, i, i, i],
          [º, º, o, º, º, º, º, º, º, º, º, i, i, i, i, i, i, i, i, i],
          [º, º, o, o, o, º, º, º, º, º, º, i, i, i, i, i, i, i, i, i],
          [º, º, º, º, o, º, º, º, o, o, o, o, i, i, i, i, i, i, i, i],
          [º, º, º, o, o, º, o, o, o, º, º, º, i, i, i, i, i, i, i, i],
          [º, º, º, o, º, º, o, º, o, º, º, º, i, i, i, i, i, i, i, i],
          [º, º, º, o, o, o, o, º, o, º, º, º, º, i, i, i, i, i, i, i],
          [º, º, º, º, o, º, º, º, o, o, º, º, º, i, i, i, i, i, i, i],
          [º, º, º, º, o, º, º, º, º, o, º, º, º, º, i, i, i, i, i, i],
          [º, º, º, º, o, o, º, º, º, o, o, º, º, º, º, º, º, º, i, i],
          [º, º, º, º, º, o, º, º, º, º, o, º, º, º, º, º, º, º, º, º],
          [º, º, º, º, o, o, o, º, º, º, o, º, º, º, º, º, º, º, º, º],
          [º, º, º, º, o, º, o, º, o, o, o, º, º, º, º, º, º, º, º, º],
          [º, º, º, o, o, º, º, º, o, º, º, º, º, º, º, º, º, º, º, º],
          [º, º, o, o, º, º, º, º, o, º, º, o, o, o, º, º, º, º, º, º],
          [º, º, o, º, º, º, º, º, o, º, º, o, º, o, o, º, º, º, º, º],
          [º, º, º, º, º, º, º, º, o, o, º, o, º, º, o, o, o, o, o, o],
          [º, º, º, º, º, º, º, º, º, o, o, o, º, º, º, º, º, º, º, o],
          [º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, o],
          [º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, º, o]
        ],
      cursorPosition = (0, 0),
      selectedIndex = 0,
      portaisJogo =
        [ Portal
            { posicaoPortal = (0, 0),
              ondasPortal =
                [ Onda
                    { cicloOnda = 5,
                      tempoOnda = 2,
                      entradaOnda = 10,
                      inimigosOnda =
                        replicate
                          10
                          Inimigo
                            { vidaInimigo = 20,
                              butimInimigo = 100,
                              velocidadeInimigo = 1,
                              direcaoInimigo = Este,
                              distDirecaoAtual = 0,
                              pathInimigo = pathToDirections $ fromJust $ dfs (mapaJogo jogo) (0, 0) (10, 10) [],
                              posicaoInimigo = (0, 0),
                              ataqueInimigo = 30,
                              projeteisInimigo = []
                            }
                    }
                ]
            },
          Portal
            { posicaoPortal = (19, 19),
              ondasPortal =
                [ Onda
                    { cicloOnda = 5,
                      tempoOnda = 2,
                      entradaOnda = 10,
                      inimigosOnda =
                        replicate
                          5
                          Inimigo
                            { vidaInimigo = 25,
                              butimInimigo = 150,
                              velocidadeInimigo = 1,
                              direcaoInimigo = Norte,
                              distDirecaoAtual = 0,
                              pathInimigo = pathToDirections $ fromJust $ dfs (mapaJogo jogo) (19, 19) (10, 10) [],
                              posicaoInimigo = (19, 19),
                              ataqueInimigo = 35,
                              projeteisInimigo = []
                            }
                    }
                ]
            }
        ],
      tempoJogo = 0
    }

janela :: Display
janela = InWindow "Immutable Towers" (1920, 1080) (0, 0)

fundo :: Color
fundo = white

fr :: Int
fr = 60

main :: IO ()
main = do
  putStrLn "Hello from Immutable Towers!"

  print $ dfs (mapaJogo jogo) (4, 0) (2, 5) []

  play janela fundo fr it desenha reageEventos reageTempo
  where
    it =
      ImmutableTowers
        { estadoJogo = jogo,
          windowSize = (1920, 1080),
          tileSize = 200
        }

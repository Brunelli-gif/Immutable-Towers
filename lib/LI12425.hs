-- |
-- Module      : LI12425
-- Description : Definições base do jogo
-- Copyright   : Nelson Estevão <d12733@di.uminho.pt>
--               Olga Pacheco   <omp@di.uminho.pt>
--               Pedro Peixoto  <d14110@di.uminho.pt>
--               Xavier Pinho   <d12736@di.uminho.pt>
--
-- Tipos de dados e funções auxiliares para a realização do projeto de LI1 em 2024/25.
module LI12425
  ( -- * Tipos de dados

    -- ** Básicos
    Creditos,
    Direcao (..),
    Distancia,
    Duracao (..),
    Posicao,
    Semente,
    Tempo,

    -- ** Mapas
    Mapa,
    Terreno (..),

    -- ** Entidades
    Base (..),
    Torre (..),
    Portal (..),
    Inimigo (..),
    TipoProjetil (..),
    Projetil (..),

    -- ** Jogo
    Jogo (..),
    Onda (..),
    Loja,

    -- * Funções auxiliares
    geraAleatorios,
    getDecimal,
  )
where

import System.Random (mkStdGen, randoms)

-- | Tipo de terrenno do mapa.
data Terreno
  = -- | Torres constroem-se sobre o relvado do mapa.
    Relva
  | -- | A base e os portais constroem-se sobre caminhos de terra do mapa. Além disso, inimigos movem-se sobre estes terrenos.
    Terra
  | -- | Água para efeito decorativo, mas onde não se pode construir, nem os inimigos se podem mover.
    Agua
  deriving (Eq)

instance (Show Terreno) where
  show Terra = show "o"
  show Agua = show "i"
  show Relva = show "º"

-- | Mapa do jogo composto por uma matriz de terrenos.
type Mapa = [[Terreno]]

-- | Coordenada bilateral de uma entidade no jogo, representante do seu centro.
-- O referencial tem origem no canto superior esquerdo, com eixos x e y positivos para a direita e para baixo, respectivamente.
type Posicao = (Float, Float)

-- | Moeda do jogo.
type Creditos = Int

-- | Base de operações do jogador.
data Base = Base
  { -- | Vida da base. Quando esta chega a zero, o jogador perde o jogo.
    vidaBase :: Float,
    -- | Posição da base no mapa. A base deve estar sobre um terreno de terra.
    posicaoBase :: Posicao,
    -- | Balanço de créditos do jogador.
    creditosBase :: Creditos
  }

instance (Show Base) where
  show base = " { Vida: " ++ show (vidaBase base) ++ ", Creds: " ++ show (creditosBase base) ++ " } "

-- | Distância entre duas posições.
type Distancia = Float

-- | Tempo em segundos.
type Tempo = Float

-- | Representa uma duração em segundos
data Duracao
  = -- | Duração em segundos
    Finita Tempo
  | -- | Duração infinita
    Infinita
  deriving (Eq, Show, Ord)

-- | A instância da Classe `Num` para `Duracao`
--
-- Apesar de técnicamente a duração ser apenas positiva ou zero,
-- esta instância permite (e opera) sobre valores negativos
-- de forma a permitir subtração de durações.
--
-- == Propriedades
--
-- === Propriedades da Duracao Infinita
--
-- > Infinita + x = x + Infinita = Infinita
--
-- A adição de qualquer valor com uma Duracao Infinita é Infinita
--
-- > Infinita * x = x * Infinita = Infinita
--
-- O mesmo aplicasse à multiplicação
--
-- === Outras Propriedades
--
-- > abs (Finita a) = Finita (abs a)
instance Num Duracao where
  (+) :: Duracao -> Duracao -> Duracao
  (+) Infinita _ = Infinita
  (+) _ Infinita = Infinita
  (+) (Finita a) (Finita b) = Finita (a + b)

  (*) :: Duracao -> Duracao -> Duracao
  (*) Infinita _ = Infinita
  (*) _ Infinita = Infinita
  (*) (Finita a) (Finita b) = Finita (a * b)

  negate :: Duracao -> Duracao
  negate Infinita = Infinita
  negate (Finita a) = Finita (-a)

  abs :: Duracao -> Duracao
  abs Infinita = Infinita
  abs (Finita a) = Finita (abs a)

  fromInteger :: Integer -> Duracao
  fromInteger a = Finita (fromInteger a)

  signum :: Duracao -> Duracao
  signum Infinita = Infinita
  signum (Finita a)
    | a > 0 = 1
    | a == 0 = 0
    | otherwise = -1

-- | Instância de Fractional para Duracao
instance Fractional Duracao where
  fromRational :: Rational -> Duracao
  fromRational t = Finita (fromRational t)

  (/) :: Duracao -> Duracao -> Duracao
  (Finita t1) / (Finita t2)
    | t2 == 0 = error "Divisão por 0 (zero)"
    | otherwise = Finita (t1 / t2)
  _ / Infinita = 0
  Infinita / _ = Infinita

-- | Instância da classe Real para Duracao
instance Real Duracao where
  toRational :: Duracao -> Rational
  toRational (Finita t) = toRational t
  toRational Infinita = error "Não é possível converter Infinita para um número real"

-- | Instância da classe RealFrac para Duracao
instance RealFrac Duracao where
  properFraction :: (Integral b) => Duracao -> (b, Duracao)
  properFraction (Finita t) =
    let (n, frac) = properFraction t
     in (n, Finita frac)
  properFraction Infinita = error "Cannot extract fraction from infinity"

-- | Retorna a parte decimal de um RealFrac
getDecimal :: (RealFrac a) => a -> a
getDecimal a = a - fromIntegral (floor a :: Int)

-- | Torre que dispara projéteis contra inimigos.
data Torre = Torre
  { -- | Posição da torre no mapa.
    posicaoTorre :: Posicao,
    -- | Redução de vida no inimigo pelo impacto do projétil.
    danoTorre :: Float,
    -- | Alcance circular da torre.
    alcanceTorre :: Float,
    -- | Número de máximo de inimigos simultaneamente atingidos por uma rajada de tiros.
    rajadaTorre :: Int,
    -- | Ciclo de tempo entre rajadas de tiros.
    cicloTorre :: Tempo,
    -- | Tempo restante para a próxima rajada de tiros.
    tempoTorre :: Tempo,
    -- | Efeito secundário associado ao tipo de projétil da torre.
    projetilTorre :: Projetil
  }

instance (Show Torre) where
  show t = " { Rajada: " ++ show (rajadaTorre t) ++ ", Tempo: " ++ show (tempoTorre t) ++ " } "

-- | Loja de torres disponíveis para construir por uma quantidade de créditos.
type Loja = [(Creditos, Torre)]

-- | Tipo de projétil disparado por uma torre.
data TipoProjetil = Fogo | Gelo | Resina
  deriving (Eq, Show)

-- | Projétil aplicado por uma torre.
data Projetil = Projetil
  { -- | Tipo de projétil.
    tipoProjetil :: TipoProjetil,
    -- | Duração do efeito do projétil no inimigo.
    duracaoProjetil :: Duracao
  }

instance (Show Projetil) where
  show proj = " { Tipo: " ++ show (tipoProjetil proj) ++ ", " ++ show (duracaoProjetil proj) ++ " } "

-- | Direção de movimento de uma entidade no jogo.
data Direcao
  = Norte
  | Sul
  | Este
  | Oeste
  | None
  deriving (Eq, Show)

-- | Inimigo que se move em direção à base do jogador.
data Inimigo = Inimigo
  { -- | Posição do inimigo no mapa.
    posicaoInimigo :: Posicao,
    -- | Direção do último movimento do inimigo.
    direcaoInimigo :: Direcao,
    -- | A distancia percorrida na direcaoInimigo.
    distDirecaoAtual :: Float,
    -- | Lista das direções a tomar
    pathInimigo :: [Direcao],
    -- | Vida do inimigo.
    vidaInimigo :: Float,
    -- | Velocidade do inimigo.
    velocidadeInimigo :: Float,
    -- | Dano causado pelo inimigo na base do jogador.
    ataqueInimigo :: Float,
    -- | Créditos que o jogador recebe ao derrotar o inimigo.
    butimInimigo :: Creditos,
    -- | Efeitos secundários ativos no inimigo.
    projeteisInimigo :: [Projetil]
  }

instance (Show Inimigo) where
  show i = " { Posição: (" ++ show (posicaoInimigo i) ++ "), " ++ show (direcaoInimigo i) ++ ", Vida: " ++ show (vidaInimigo i) ++ ", Projeteis" ++ show (projeteisInimigo i) ++ " } "

-- | Onda de inimigos que saem de um portal.
data Onda = Onda
  { -- | Inimigos que compõem a onda.
    inimigosOnda :: [Inimigo],
    -- | Tempo em segundos entre a entrada de cada inimigo.
    cicloOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada do próximo inimigo da onda.
    tempoOnda :: Tempo,
    -- | Tempo restante, em segundos, para a entrada da onda.
    entradaOnda :: Tempo
  }
  deriving (Show)

instance Eq Onda where
  (==) :: Onda -> Onda -> Bool
  (==) o1 o2 = entradaOnda o1 == entradaOnda o2 && tempoOnda o1 == tempoOnda o2 && cicloOnda o1 == cicloOnda o2

-- | Compara a entrada da `Onda` (`entradaOnda`) e ordena a `Onda` de acordo com isso.
--
-- Ondas com entrada menor são menores que outras Ondas de entradas maiores.
instance Ord Onda where
  compare :: Onda -> Onda -> Ordering
  compare o1 o2
    | entradaOnda o1 > entradaOnda o2 = GT
    | entradaOnda o1 == entradaOnda o2 = EQ
    | otherwise = LT

-- | Portal de entrada de inimigos no mapa.
data Portal = Portal
  { -- | Posição do portal no mapa. O portal deve estar sobre um terreno de terra.
    posicaoPortal :: Posicao,
    -- | Ondas de inimigos que saem do portal.
    ondasPortal :: [Onda]
  }
  deriving (Show)

-- | Estado do jogo. Um jogo é composto pela base, vários portais, várias torres, um mapa, vários inimigos e a loja.
data Jogo = Jogo
  { -- | Base de operações do jogador.
    baseJogo :: Base,
    -- | Portais de entrada de inimigos no mapa.
    portaisJogo :: [Portal],
    -- | Torres construídas pelo jogador.
    torresJogo :: [Torre],
    -- | Mapa retangular do jogo.
    mapaJogo :: Mapa,
    -- | Inimigos em movimento no mapa.
    inimigosJogo :: [Inimigo],
    -- | Loja de torres disponíveis para construir.
    lojaJogo :: Loja,
    -- | Tempo Total decorrido
    tempoJogo :: Tempo,
    -- | A torre selecionada para colocar
    selectedIndex :: Int,
    -- | A posição do cursor para colocar uma torre
    cursorPosition :: (Int, Int),
    -- | Determina se o jogo está em pausa, apenas para processamento do estado do Jogo
    pausa :: Bool
  }
  deriving (Show)

-- | Valor inicial que determina a sequência de números pseudo-aleatórios.
type Semente = Int

-- | Função que gera uma lista de números aleatórios a partir de uma 'Semente'.
--
-- == Exemplos
--
-- >>> geraAleatorios 2425 3
-- [9108974057934916489,3509742222561512871,1534041518507426227]
--
-- >>> geraAleatorios 10 1
-- [3575835729477015470]
geraAleatorios :: Semente -> Int -> [Int]
geraAleatorios s c = take c $ randoms (mkStdGen s)

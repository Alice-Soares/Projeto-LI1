{-|
Module      : DadosParaGloss
Description : Define tipos de dados usados no Gloss.
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Tipos de dados e funções auxiliares para a realização da parte gráfica do projeto de LI1 em 2023/24.

-}

module DadosParaGloss where 

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4



-- | Definição base para o EstadoDaApp
data EstadoDaApp = EstadoDaApp {
    tempoTotal :: Float, -- ^ Tempo total desde que começa o jogo.
    ecranAtual :: Ecran, -- ^ Ecran que está aberto.
    menu :: Menu,
    jogo :: Jogo,
    niveis :: [Nivel],
    nivelAtual :: Int, -- ^ mapa do jogo em caracteres 
    blocos :: BlocosPic, -- ^ Imagens dos blocos.
    personagensPic :: PersonagensPic, -- ^ Imagens dos personagens.
    colecionaveisPic :: ColecionaveisPic,
    infoPic :: InfoPic,
    menuPic :: MenuPic,
    larguraJanela :: Int, 
    alturaJanela :: Int
} deriving (Eq, Show)

-- | Níveis

type Nivel = [[Char]]

-- | Estado do Menu
data Menu  = Menu {
    itens :: [(ItemMenu,Picture)],
    itemSelecionado :: ItemMenu
} deriving (Eq, Show)

-- | Items Do Menu
data ItemMenu = IniciarJogo1 | IniciarJogo2 | Creditos | Sair
                deriving (Eq, Show, Enum, Bounded)

-- | Imagens dos items do menu
data MenuPic = MenuPic 
 {
    botoesPic :: [Picture],
    tituloPic :: Picture
 } deriving (Eq, Show)


-- | Tipos de Ecrans
data Ecran = EcranMenu | EcranJogo | EcranGameOver | EcranWin | EcranCreditos
                deriving (Eq, Show, Enum, Bounded)

-- | Imagens para informação variada

data InfoPic = InfoPic 
  {
  gameOverPic :: Picture,
  winPic      :: Picture,
  vidaPic     :: Picture,
  numerosPic  :: [Picture],
  estrelaPic  :: Picture,
  creditosPic :: Picture  

  } deriving (Eq, Show)

-- | Imagens dos blocos
data BlocosPic = BlocosPic
  {
    blocoPlataforma :: Picture,
    blocoVazio :: Picture,
    blocoEscada :: Picture,
    blocoAlcapao :: Picture
  } deriving (Eq, Show)

-- | Imagens dos personagens
data PersonagensPic = PersonagensPic
  {
    jogadorAndarPic :: [Picture],
    jogadorSubirPic :: [Picture],
    jogadorArmadoPic :: [Picture],
    fantasmaPic ::[Picture],
    macacoMalvadoPic :: [Picture]
  } deriving (Eq, Show)

-- | Imagens dos colecionáveis

data ColecionaveisPic = ColecionaveisPic
  {
    marteloPic :: Picture,
    moedaPic :: Picture
  } deriving (Eq, Show)

-- | Número de pixeis por Bloco (Pixeis por unidade)
pixeisPorBloco :: Int
pixeisPorBloco = 32

-- | Posição inicial, no eixo dos X, da Janela no ecran do computador
posicaoXInicialDajanela :: Int
posicaoXInicialDajanela = 100

-- | Posição inicial, no eixo dos Y, da Janela no ecran do computador
posicaoYInicialDajanela :: Int 
posicaoYInicialDajanela = 100

{-| Funções que fazem a conversão das coordenadas (x,y) do referencial para o gloss,
visto que o gloss tem o ponto (0,0) no centro da janela e queremos "transferi-lo" para o canto superior esquerdo.
-}

cantoSuperiorEsquerdoX :: Int ->  Float
cantoSuperiorEsquerdoX larguraJanela  = -((fromIntegral larguraJanela) / 2)

cantoSuperiorEsquerdoY :: Int -> Float
cantoSuperiorEsquerdoY  alturaJanela = ((fromIntegral alturaJanela) / 2) 

{-| Funções que fazem a conversão dos valores das coordenadas (x,y) do referencial para o gloss,
visto que o gloss é em pixeis e o referencial toma como uma unidade um bloco.
-}

xParaGloss :: Double -> Float
xParaGloss x = realToFrac (x * fromIntegral pixeisPorBloco)

yParaGloss :: Double -> Float
yParaGloss y = - realToFrac (y * fromIntegral pixeisPorBloco)
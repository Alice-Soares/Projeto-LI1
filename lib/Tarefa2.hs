{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1
import GHC.Event.TimeOut (TimeoutKey(TK))
import GHC.Read (paren)
import Data.List (group)
import System.Random (Random(random))
import  GHC.TypeLits 

{-|
= Validação do Jogo

A função valida confirma se um jogo é válido.

-}

valida :: Jogo -> Bool
valida (Jogo mapa inimigos colecionaveis jogador) = validaChao matrizDeBlocos &&
              validaRessalto inimigos jogador &&
              validarPosicaoInicialPersonagens inimigos jogador &&
              validarNumeroDeInimigos inimigos &&
              validarVidaFantasmas inimigos &&
              validaEscadas matrizDeBlocos &&
              validarLarguraAlcapoes matrizDeBlocos jogador &&
              validarPosicaoInicialEmVazio jogador colecionaveis matrizDeBlocos

              where
               Mapa _ _  matrizDeBlocos = mapa


{-|

1. A função validaChao valida se o mapa tem chão, impedindo os personagens de cairem fora do mapa.

-}


validaChao :: [[Bloco]] -> Bool
validaChao m = all (==Plataforma) (last m) -- ^ Intende-se por chão que a última lista da matriz de blocos é composta apenas por blocos de Plataforma.

{-|

2. A função validaRessalto valida a propriedade ressalta dos personagens, validando em separado o jogador e os inimigos, 
pois estes têm a propriedade ressalta diferente.

-}


validaRessalto :: [Personagem] -> Personagem -> Bool
validaRessalto inimigos jogador = validarRessaltoInimigos inimigos && validarRessaltoJogador jogador

validarRessaltoInimigos :: [Personagem] -> Bool
validarRessaltoInimigos = all ressalta -- ^ Todos os inimigos têm a propriedade ressalta a True.
-- Versão não simplicada:
-- validarRessaltoInimigos inimigos = all (\i -> ressalta i == True) inimigos

validarRessaltoJogador :: Personagem -> Bool
validarRessaltoJogador = not . ressalta -- ^ O jogador tem a propriedade ressalta a False.

{-|

3. A função validarPosicaoInicialPersonagens valida que a posição inicial de um jogador não colide com a posição inicial de outro personagem.
Foi usado a colisaoPersonagens em vez de comparar as posições exatas dos personagens, pois estas podiam ser diferentes e os personagens colidirem na mesma.
-}

validarPosicaoInicialPersonagens :: [Personagem] -> Personagem -> Bool
validarPosicaoInicialPersonagens inimigos jogador = all (\inimigo -> not (colisoesPersonagens inimigo jogador)) inimigos

-- validarPosicaoInicialPersonagens inimigos jogador = all (\inimigo -> (colisoesPersonagens inimigo jogador) == False) inimigos                  

{-|
4. A função validarNumeroDeInimigos valida um número mínimo de inimigos.
-}


validarNumeroDeInimigos :: [Personagem] -> Bool
validarNumeroDeInimigos inimigos = length inimigos >= 2 -- ^ Número mínimo de inimigos é 2.

{-|
5. A função validarVidaFantasmas valida que os fantasmas têm exatamente 1 vida.

-}

validarVidaFantasmas :: [Personagem] -> Bool
validarVidaFantasmas inimigos = all (\f -> vida f == 1 ) fantasmas
                                where
                                    fantasmas = filter (\p -> tipo p == Fantasma) inimigos -- ^ Filtra apenas os fantasmas da lista de inimigos.

{-|
6. A função validaEscadas confirma se as escadas são válidas, ou seja, se elas respeitam as seguintes condições:

* Não começam nem terminam em Alçapões;
* Uma das extremidades tem que ser do tipo Plataforma.

No entanto, se o bloco Escada estiver no meio de dois blocos Escada, estas também serão válidas.


-}

validaEscadas :: [[Bloco]] -> Bool
validaEscadas mapa = and [temDuasEscadasNasExtremidades (x,y,b) || (naoComecaNemTerminaEmA (x,y,b) && umaDasExtermidadesComP (x,y,b)) | (x,y,b) <- ziparMatrizComIndices mapa, b == Escada ]
    where
        temDuasEscadasNasExtremidades (x,y,b) = (blocoAnterior x y mapa == Escada) && (blocoSeguinte x y mapa == Escada)
        naoComecaNemTerminaEmA (x,y,b) = (blocoAnterior x y mapa /= Alcapao) && (blocoSeguinte x y mapa /= Alcapao)
        umaDasExtermidadesComP (x,y,b) = blocoAnterior x y mapa == Plataforma || blocoSeguinte x y mapa == Plataforma
        blocoAnterior x y mapa = if x == 0 then Vazio else mapa !! (x-1) !! y
        blocoSeguinte x y mapa = if x == length mapa - 1 then Vazio else mapa !! (x+1) !! y

{-|
Função auxiliar para ter uma lista de triplos com os indices dos blocos na matriz.
Os indices são necessários para, na função validaescadas, saber qual o bloco anterior e seguinte.
-}

ziparMatrizComIndices :: [[Bloco]] -> [(Int,Int,Bloco)]
ziparMatrizComIndices matriz =  concat (zipWith zipLinha [0..] matriz)
    where
        zipLinha indiceLinha = zipWith (zipBloco indiceLinha) [0..]
        zipBloco indiceLinha indiceColuna bloco = (indiceLinha,indiceColuna,bloco)

{-|
>>> ziparMatrizComIndices [[E,E,E],[E,E,E],[E,E,E]] 
[(0,0,E),(0,1,E),(0,2,E),(1,0,E),(1,1,E),(1,2,E),(2,0,E),(2,1,E),(2,2,E)]
-}

{-|
7. A função validarLarguraAlcapoes valida que os alçapões são mais largos que o jogador.
Tendo em conta a possibilidade de haver mais do que um alçapão seguido, foram utilizadas funções auxiliares para obter os conjuntos de alçapões.
-}


validarLarguraAlcapoes :: [[Bloco]] -> Personagem -> Bool
validarLarguraAlcapoes mapa personagem  = all (\l -> fromIntegral (length l) > larguraPersonagem) listaDeConjuntosDeAlcapoes
                                        where
                                            (larguraPersonagem,_) = tamanho personagem
                                            listaDeConjuntosDeAlcapoes = conjuntoDeAlcapoes (conjuntosDeBlocosIguais mapa)


conjuntosDeBlocosIguais :: Eq a => [[a]] -> [[a]]
conjuntosDeBlocosIguais = concatMap group
-- conjuntosDeBlocosIguais mapa = concat (map group mapa)

conjuntoDeAlcapoes :: [[Bloco]] -> [[Bloco]]
conjuntoDeAlcapoes = filtraPorConjuntoDe Alcapao
-- conjuntoDeAlcapoes mapa = filter (\l -> head l == Alcapao ) mapa

filtraPorConjuntoDe ::  Bloco -> [[Bloco]] -> [[Bloco]]
filtraPorConjuntoDe bloco = filter (\l -> head l == bloco)
-- filtraPorConjuntoDe  bloco mapa = filter (\l -> head l == bloco) mapa

{-|
7. A função validarPosicaoInicialEmVazio valida que as personagens e os colecionáveis iniciam num bloco Vazio.
-}


validarPosicaoInicialEmVazio :: Personagem -> [(Colecionavel, Posicao)] -> [[Bloco]] -> Bool
validarPosicaoInicialEmVazio p colecionaveis m = validarPosicaoInicialPersonagemEmVazio p m && validarPosicaoInicialColecionaveisEmVazio colecionaveis m

{-| Funções auxiliares que validam as posições iniciais no vazio em separado.
-}

validarPosicaoInicialPersonagemEmVazio :: Personagem -> [[Bloco]] -> Bool
validarPosicaoInicialPersonagemEmVazio p m = all (== Vazio) (blocosDentroDaHitboxDePersonagem m p)


validarPosicaoInicialColecionaveisEmVazio :: [(Colecionavel, Posicao)] -> [[Bloco]] -> Bool
validarPosicaoInicialColecionaveisEmVazio colecionaveisComPosicoes mapa =  all (\cp -> validarPosicaoDoColecionavel cp mapa )  colecionaveisComPosicoes
                                                                where
                                                                    validarPosicaoDoColecionavel cp mapa = all (==Vazio) (blocosColidemColecionavel cp mapa)

{-| Função auxiliar semelhante à função blocosDentroDaHitboxDePersonagem adaptada para os colecionáveis.
-}

blocosColidemColecionavel :: (Colecionavel, Posicao) -> [[Bloco]] -> [Bloco]
blocosColidemColecionavel (_,(x,y)) mapa = [mapa !! xi !! yi | xi <-  [xInferior..xSuperior-1], yi <- [yInferior..ySuperior-1]]
                                            where
                                                xInferior = max 0 (floor x)
                                                xSuperior = xInferior + 1
                                                yInferior = max 0 (floor y)
                                                ySuperior = yInferior + 1

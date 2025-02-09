{-| 
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe

import LI12324
import Tarefa1
import Tarefa2
import Tarefa3 
import  GHC.TypeLits 




atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza acoesSobreInimigos acoesSobreJogador jogo = let
                                                        Mapa _ _ matrizDeBlocos = mapa jogo
                                                        jogadorAtualizado = atualizaJogador acoesSobreJogador jogo
                                                    in
                                                        jogo { 
                                                            jogador = jogadorAtualizado
                                                        }

-- | Constante utilizada para o movimento dos personagens.

velocidadeMovimento :: Double
velocidadeMovimento = 2


atualizaJogador :: Maybe Acao -> Jogo -> Personagem
atualizaJogador acao jogo =  let
                                    (vx, vy) = velocidade jogadorDoJogo
                                    jogadorDoJogo = jogador jogo
                                 in
                                    case acao of
                                        Just AndarDireita -> aplicaAndarDireita jogo 
                                        Just AndarEsquerda -> aplicaAndarEsquerda jogo
                                        Just Subir -> aplicaSubir jogo
                                        Just Descer -> aplicaDescer jogo
                                        Just Saltar -> aplicaSalto jogo
                                        Just Parar -> aplicaParar jogo
                                        _ -> jogadorDoJogo

aplicaParar ::  Jogo -> Personagem
aplicaParar jogo = let
                        (vx,vy) = velocidade jogadorDoJogo
                        jogadorEmEscada = emEscada jogadorDoJogo
                        jogadorDoJogo = jogador jogo
                        Mapa _ _ matrizDeBlocos = mapa jogo

                      in 
                        if personagemEmCimaDeBloco jogadorDoJogo Plataforma matrizDeBlocos || personagemEmCimaDeBloco jogadorDoJogo Escada matrizDeBlocos -- ^ Para se não estiver no ar
                        then jogadorDoJogo { velocidade = (0,0) }
                        else jogadorDoJogo { velocidade = (0,vy) } -- ^ Apenas para no eixo dos X se estiver no ar

-- | Velocidade aplicada no eixo dos Y no momento em que salta.

velocidadeDeSalto:: Double
velocidadeDeSalto = 6.5

aplicaSalto ::  Jogo -> Personagem
aplicaSalto jogo = let
                        (vx,vy) = velocidade jogadorDoJogo
                        jogadorEmEscada = emEscada jogadorDoJogo
                        jogadorDoJogo = jogador jogo
                        Mapa _ _ matrizDeBlocos = mapa jogo
                      in 
                        if not jogadorEmEscada && (personagemEmCimaDeBloco jogadorDoJogo Plataforma matrizDeBlocos || personagemEmCimaDeBloco jogadorDoJogo Alcapao matrizDeBlocos)
                        then jogadorDoJogo { velocidade = (vx, -velocidadeDeSalto) } -- ^ velocidade fica negativa no y por causa do referencial
                        else jogadorDoJogo

aplicaAndarDireita ::  Jogo -> Personagem
aplicaAndarDireita jogo = let
                        jogadorDoJogo = jogador jogo
                        (vx,vy) = velocidade jogadorDoJogo
                        jogadorEmEscada = emEscada jogadorDoJogo
                        Mapa _ _ matrizDeBlocos = mapa jogo
                      in 
                        if not jogadorEmEscada || personagemEmCimaDeBloco jogadorDoJogo Plataforma matrizDeBlocos -- ^ Não anda para a direita se estiver a usar a escada.
                        then jogadorDoJogo { velocidade = (velocidadeMovimento, vy), direcao = Este }
                        else jogadorDoJogo

aplicaAndarEsquerda ::  Jogo -> Personagem
aplicaAndarEsquerda jogo = let
                        jogadorDoJogo = jogador jogo
                        (vx,vy) = velocidade jogadorDoJogo
                        jogadorEmEscada = emEscada jogadorDoJogo
                        Mapa _ _ matrizDeBlocos = mapa jogo
                      in 
                        if not jogadorEmEscada || personagemEmCimaDeBloco jogadorDoJogo Plataforma matrizDeBlocos -- ^ Não anda para a esquerda se estiver a usar a escada.
                        then jogadorDoJogo { velocidade = (-velocidadeMovimento, vy), direcao = Oeste }
                        else jogadorDoJogo

aplicaSubir ::  Jogo -> Personagem
aplicaSubir jogo = let
                        (vx,vy) = velocidade jogadorDoJogo
                        jogadorEmEscada = emEscada jogadorDoJogo
                        jogadorDoJogo = jogador jogo
                        Mapa _ _ matrizDeBlocos = mapa jogo
                      in 
                        if jogadorEmEscada -- ^ Apenas sobe se estiver em escada.
                        then jogadorDoJogo { velocidade = (0, -velocidadeMovimento), direcao = Norte }
                        else jogadorDoJogo

aplicaDescer ::  Jogo -> Personagem
aplicaDescer jogo = let
                        (vx,vy) = velocidade jogadorDoJogo 
                        jogadorEmEscada = emEscada jogadorDoJogo
                        jogadorDoJogo = jogador jogo
                        Mapa _ _ matrizDeBlocos = mapa jogo
                      in 
                        if jogadorEmEscada -- ^ Apenas desce se estiver em escada.
                        then jogadorDoJogo { velocidade = (0, velocidadeMovimento), direcao = Sul}
                        else jogadorDoJogo



-- | Averigua se o personagem encontra-se em uma escada tendo em conta a matriz de blocos e a posição do personagem.

personagemEmEscada :: Personagem -> [[Bloco]] -> Bool
personagemEmEscada jogador matrizDeBlocos = matrizDeBlocos !! y !! x == Escada
                                        where 
                                            (xs,ys) = posicao jogador
                                            x = floor xs 
                                            y = floor ys


-- | Se a velocidade vertical for nula, então o personagem não está em movimento na componente y, ou seja, na vertical.
semVelocidadeVertical :: Personagem -> Bool
semVelocidadeVertical personagem = snd (velocidade personagem) == 0







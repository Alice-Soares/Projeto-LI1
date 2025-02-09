{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

{-|
= Verificação de colisões 

 == Hitboxes

A função hitBoxDePersonagem calcula a hitbox do personagem a partir da sua posição e tamanho.
 -}

hitBoxDePersonagem :: Personagem -> Hitbox
hitBoxDePersonagem  p =  ((xp-(tx/2),yp-(ty/2)),(xp+(tx/2),yp+(ty/2)))
    where (xp,yp) = posicao p
          (tx,ty) = tamanho p

{-| A função hitBoxDeBloco calcula a hitbox dos blocos da matriz a partir do seu índice.
 -}

hitBoxDeBloco :: (Int, Int) -> Hitbox
hitBoxDeBloco (x,y) = ((fromIntegral x, fromIntegral y), (fromIntegral x + 1, fromIntegral y + 1))

{-| == Colisões

A função /colisõesComParede/ usa duas funções auxiliares para verificar se um personagem está a colidir com:

 * o limite do mapa;
 * algum bloco de plataforma. 

 -}


colisoesParede ::  Mapa -> Personagem  ->  Bool
colisoesParede mapa personagem  = colideComLimiteDoMapa personagem mapa || colideComBloco Plataforma mapa personagem

colideComLimiteDoMapa :: Personagem -> Mapa -> Bool
colideComLimiteDoMapa personagem mapa =  xInferior < 0 || yInferior < 0 || xSuperior >  largura || ySuperior > altura
                                     where  ((xInferior,yInferior), (xSuperior, ySuperior)) = hitBoxDePersonagem personagem
                                            largura =  fromIntegral (length (head matrizDeBlocos))
                                            altura =  fromIntegral (length matrizDeBlocos) 
                                            Mapa _ _ matrizDeBlocos = mapa


{-| Função mais geral para poder utilizar com diferentes blocos. -}

colideComBloco :: Bloco -> Mapa ->  Personagem -> Bool
colideComBloco bloco mapa personagem = bloco `elem` blocos
        where 
            blocos = blocosDentroDaHitboxDePersonagem matrizDeBlocos personagem
            Mapa _ _ matrizDeBlocos = mapa

{-| Função auxiliar que cria uma lista dos blocos que estão dentro da hitbox do personagem. -}

blocosDentroDaHitboxDePersonagem :: [[Bloco]] -> Personagem -> [Bloco]
blocosDentroDaHitboxDePersonagem matrizDeBlocos personagem = [matrizDeBlocos !! y !! x | x <- [xInferior..xSuperior], y <- [yInferior..ySuperior]]
                                 where  ((xi,yi),(xs,ys)) = hitBoxDePersonagem personagem
                                        larguraDaMatriz = fromIntegral (length (head matrizDeBlocos))
                                        alturaDaMatriz = fromIntegral (length matrizDeBlocos)
                                        xInferior = max 0 ( floor xi)
                                        yInferior = max 0 (floor yi)
                                        xSuperior = min larguraDaMatriz (floor xs)
                                        ySuperior = min alturaDaMatriz (floor ys)


{-| A função colisoesPersonagens confirma se dois personagens estão a colidir a partir das suas hitboxes.
 -}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoEntreHitboxes hb1 hb2
                            where
                                hb1 = hitBoxDePersonagem p1
                                hb2 = hitBoxDePersonagem p2

colisaoEntreHitboxes :: Hitbox -> Hitbox -> Bool
colisaoEntreHitboxes hb1 hb2 = xInferior1 <= xSuperior2 && xSuperior1 >= xInferior2 && yInferior1 <= ySuperior2 && ySuperior1 >= yInferior2
    where
        ((xInferior1,yInferior1), (xSuperior1,ySuperior1)) = hb1
        ((xInferior2,yInferior2), (xSuperior2,ySuperior2)) = hb2






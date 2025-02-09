{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Tarefa1
import Tarefa2
import qualified GHC.TypeLits as Y


{-| A função movimenta vai atualizando os inimigos, jogador, mapa, colecionaveis conforme as funções utilizadas, movimentando o jogo. 


-}

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta semente variacaoTempo jogo = jogo {
                                                jogador = jogadorAtualizado,
                                                inimigos = inimigosAtualizados,
                                                mapa = mapaAtualizado,
                                                colecionaveis = colecionaveisAtualizados
                                            }
                                        where
                                            jogadorAtual = jogador jogo
                                            inimigosAtuais = inimigos jogo
                                            mapaAtual = mapa jogo
                                            colecionaveisAtuais = colecionaveis jogo
                                            jogadorAtualizado = tempoDeDano variacaoTempo $
                                                                aplicarColecionaveisAoJogador colecionaveisAtuais $
                                                                aplicaDanoDeTodosInimigosNoJogador mapaAtual inimigosAtuais $
                                                                atualizaEmEscada mapaAtual $
                                                                aplicaEfeitoGravidade variacaoTempo $
                                                                pararSeCentradoComEscada mapaAtual $
                                                                movimentaJogadorY variacaoTempo mapaAtual $
                                                                movimentaJogadorX variacaoTempo mapaAtual $
                                                                jogadorAtual
                                            inimigosAtualizados = saemDoMapaTodosInimigosMortos mapaAtual $
                                                                 atualizaVidaDeInimigos (map (movimentaInimigosX semente variacaoTempo mapaAtual jogadorAtualizado) inimigosAtuais) $
                                                                 jogadorAtual
                                            mapaAtualizado = jogadorPisaAlcapao jogadorAtual mapaAtual
                                            colecionaveisAtualizados = retirarColecionaveisQueColidemComPersonagem jogadorAtual colecionaveisAtuais

{-| A função movimentaInimigosX trata do movimento dos inimigos no eixo dos X, fazendo com que estes ressaltem se coliderem com um parede ou 
se chegarem ao fim da Plataforma. Não fizemos função para movimentar nos eixos do Y porque os inimigos apenas andam de um lado para o outro.
-}

movimentaInimigosX :: Semente -> Tempo -> Mapa -> Personagem -> Personagem -> Personagem
movimentaInimigosX  semente variacaoTempo mapa jogador inimigo =
                                        let
                                            inimigoAtualizado = movimentaInimigosX' variacaoTempo inimigo
                                            direcaoInimigo = direcao inimigo
                                            (velocidadeXInimigo,_) = velocidade inimigo
                                            Mapa _ _ matrizDeBlocos = mapa
                                            (x,y) = posicao jogador
                                            posicaoDeDecisao =  (floor (x*30)  `mod` 100) == 0
                                            vira = even(head(geraAleatorios (semente) 1))
                                            in
                                            if  colisoesParede mapa inimigoAtualizado  || 
                                                personagemEmCimaDeBloco inimigoAtualizado Vazio matrizDeBlocos ||
                                                (posicaoDeDecisao  && vira)
                                            then inimigo { direcao = if direcaoInimigo == Este then Oeste else Este, velocidade = (-velocidadeXInimigo,0)} -- adicionar funcao com a velociade do inimigo
                                            else inimigoAtualizado

{-| A função movimentaInimigosX' varia a posição dos inimigos no eixo X tendo em conta a sua velocidade e a variação de tempo, fazendo com que eles andem. 
-}

movimentaInimigosX' :: Tempo -> Personagem -> Personagem
movimentaInimigosX' variacaoTempo inimigo = inimigo {
                                        posicao = (x + vx * variacaoTempo, y)
                                        }
                                    where
                                        (x,y) = posicao inimigo
                                        (vx,vy) = velocidade inimigo

{-| A função movimentaJogadorX trata do movimento do jogador no eixo dos X, fazendo com que este pare se colidir com uma parede ou atualizando a posição do personagem.
-}

movimentaJogadorX :: Tempo -> Mapa -> Personagem -> Personagem
movimentaJogadorX variacaoTempo mapa jogador =
                        let
                            jogadorAtualizado = movimentaJogadorX' variacaoTempo jogador
                        in
                            if colisoesParede mapa jogadorAtualizado
                            then jogador
                            else jogadorAtualizado

{-| A função movimentaJogadorX' varia a posição do jogador no eixo X tendo em conta a sua velocidade e a variação de tempo, fazendo com que ele ande. 
-}

movimentaJogadorX' :: Tempo -> Personagem -> Personagem
movimentaJogadorX' variacaoTempo jogador = jogador {
                                        posicao = (x + vx * variacaoTempo, y)
                                        }
                                    where
                                        (x,y) = posicao jogador
                                        (vx,vy) = velocidade jogador

{-| A função movimentaJogadorY trata do movimento do jogador no eixo dos Y.
Como o jogador sofre sempre gravidade, ou seja, tem sempre velocidade em y, apenas movimenta se a posição seguinte do jogador não colidir com uma parede.
-}

movimentaJogadorY :: Tempo -> Mapa -> Personagem -> Personagem
movimentaJogadorY variacaoTempo mapa jogador =
                        let
                            jogadorEmNovaPosicaoY = movimentaJogadorY' variacaoTempo jogador
                            (vx,vy) = velocidade jogador
                            jogadorNaMesmaPosicaoComVelocidadeYZero = jogador { velocidade = (vx,0) }
                        in
                            if not (colisoesParede mapa jogadorEmNovaPosicaoY )
                            then jogadorEmNovaPosicaoY
                            else jogadorNaMesmaPosicaoComVelocidadeYZero

{-| A função movimentaJogadorY' varia a posição do jogador no eixo Y tendo em conta a sua velocidade e a variação de tempo. 
-}

movimentaJogadorY' :: Tempo -> Personagem -> Personagem
movimentaJogadorY' variacaoTempo jogador = jogador {
                                        posicao = (x, y + vy * variacaoTempo)
                                        }
                                    where
                                        (x,y) = posicao jogador
                                        (vx,vy) = velocidade jogador

{-| A função atualizaEmEscada atualiza o parametro que diz se o jogador está em escada ou não.
-}

atualizaEmEscada :: Mapa -> Personagem -> Personagem
atualizaEmEscada mapa jogador | colideComBloco Escada mapa jogador = jogador {
                                                                        emEscada = True
                                                                        }
                              | otherwise = jogador { emEscada = False }

{-| A função pararSeCentradoComEscada faz com que o jogador pare quando está centrado numa escada para conseguir subi-la,
pois ele só pode subir quando estiver centrado.
-}

pararSeCentradoComEscada :: Mapa -> Personagem -> Personagem
pararSeCentradoComEscada  mapa jogador = let
                                          jogadorCentradoEmEscada = centradoEmEscada mapa jogador
                                          jogadorNaDirecaoEsteOeste = (direcao jogador == Este) || (direcao jogador == Oeste)
                                          (vx,vy) = velocidade jogador
                                          (x,y) = posicao jogador
                                         in
                                            if jogadorCentradoEmEscada && jogadorNaDirecaoEsteOeste
                                            then jogador {
                                                velocidade = (0,0)
                                                 }
                                            else jogador



{-| A função centradoEmEscada verifica se o personagem está no centro da escada, tendo sido aplicada uma margem de 0.01 para cada lado.
 -}

centradoEmEscada :: Mapa -> Personagem  -> Bool
centradoEmEscada  (Mapa _ _ matrizDeBlocos) jogador  = (matrizDeBlocos !! y !! x == Escada) && restox >= 0.49  && restox <= 0.51
                                        where
                                            (xs,ys) = posicao jogador
                                            x = floor xs
                                            y = floor ys
                                            restox = xs - fromIntegral (floor xs)

{-| A função aplicaEfeitoGravidade aplica o efeito da gravidade ao jogador durante o jogo
 -}

aplicaEfeitoGravidade :: Tempo -> Personagem -> Personagem
aplicaEfeitoGravidade tempo jogador@Personagem{ emEscada = True } = jogador
aplicaEfeitoGravidade tempo jogador = jogador { velocidade = (vx, nvy) }

                                where
                                    (vx,vy) = velocidade jogador
                                    nvy  = vy + (snd gravidade) * tempo -- vf = vi + a * t

{-| A função auxiliar personagemEmCimaDeBloco determina se o bloco está por cima de um determinado bloco.
 -}


personagemEmCimaDeBloco :: Personagem -> Bloco -> [[Bloco]] -> Bool
personagemEmCimaDeBloco personagem bloco matrizDeBlocos = matrizDeBlocos !! yinferior1 !! xinferior1 == bloco || matrizDeBlocos !! yinferior2 !! xinferior2 == bloco
                                                where
                                                    ((xi,yi),(xs,ys)) = hitBoxDePersonagem personagem
                                                    (_,t) = tamanho personagem 
                                                    xinferior1 = floor xs
                                                    yinferior1 = floor (ys+(t/2))-- ^ é somado 1 aos y para verificar se o pixel abaixo da hitbox é o bloco pretendido
                                                    xinferior2 = floor xi
                                                    yinferior2 = floor (ys+(t/2))

{-| A função aplicaDanoDeTodosInimigosNoJogador aplica o dano causado pelos inimigos no jogador, utilizando uma função auxiliar que lhe retira uma vida 
e retorna o jogador á posição inicial.
 -}

aplicaDanoDeTodosInimigosNoJogador :: Mapa -> [Personagem] -> Personagem  -> Personagem
aplicaDanoDeTodosInimigosNoJogador mapa [] jogador = jogador
aplicaDanoDeTodosInimigosNoJogador mapa (inimigo:t) jogador = aplicaDanoDeTodosInimigosNoJogador mapa t (aplicaDanoDeUmInimigoNoJogador mapa jogador inimigo)


aplicaDanoDeUmInimigoNoJogador :: Mapa -> Personagem -> Personagem -> Personagem
aplicaDanoDeUmInimigoNoJogador mapa jogador inimigo | colisaoEntreHitboxes (hitBoxDePersonagem jogador) (hitBoxDePersonagem inimigo) = jogador { vida = max 0 (vida jogador - 1), posicao = (x,y) }
                                                    | otherwise = jogador
                                               where
                                                    Mapa ((x,y),_) _ _ = mapa

{-| A função jogadorPisaAlcapao utiliza a função auxiliar aplicaEfeitoAlcapao, que faz desaparecer o alçapão, quando o jogador o pisa.
 -}

jogadorPisaAlcapao :: Personagem -> Mapa -> Mapa
jogadorPisaAlcapao jogador (Mapa x y matrizDeBlocos) = Mapa x y (map (\linha -> map (\bloco -> aplicaEfeitoAlcapao jogador bloco) linha) matrizDeBlocosComPosicoes)
                                            where
                                                matrizDeBlocosComPosicoes = zipWith (\linha y -> zipWith (\bloco x -> ((x,y), bloco)) linha [0..]) matrizDeBlocos [0..]
{-| A função jogadorPisaAlcapao utiliza a função auxiliar aplicaEfeitoAlcapao, que faz desaparecer o alçapão, quando o jogador o pisa.
 -}

aplicaEfeitoAlcapao :: Personagem -> ((Int,Int), Bloco) -> Bloco
aplicaEfeitoAlcapao jogador ((x,y), bloco) | bloco == Alcapao && y == yinferior1 && x == xinferior1 = Vazio
                                           | bloco == Alcapao && y == yinferior2 && x == xinferior2 = Vazio
                                           | otherwise = bloco
                                           where
                                                ((xi,yi),(xs,ys)) = hitBoxDePersonagem jogador
                                                xinferior1 = floor xs
                                                yinferior1 = floor ys
                                                xinferior2 = floor xi
                                                yinferior2 = floor ys

{-| A função atualizaVidaDeInimigos utiliza a função auxiliar aplicaDanoEmInimigo quando o jogador está armado, retirando-lhe uma vida.
 -}


atualizaVidaDeInimigos :: [Personagem] -> Personagem -> [Personagem]
atualizaVidaDeInimigos inimigos jogador | not (jogadorArmado jogador) = inimigos
                                        | otherwise =  map (aplicaDanoEmInimigo jogador) inimigos
-- | otherwise =  map (\i -> aplicaDanoEmInimigo jogador i) inimigos

aplicaDanoEmInimigo :: Personagem -> Personagem -> Personagem
aplicaDanoEmInimigo jogador inimigo | colisaoEntreHitboxes (hitboxDeDano jogador) (hitBoxDePersonagem inimigo) = inimigo { vida = max 0 (vida inimigo - 1) }
                                    | otherwise = inimigo
-- | Função auxiliar que confirma se o jogador está armado.
jogadorArmado :: Personagem -> Bool
jogadorArmado jogador = dano && tempoRestante > 0 -- ^ jogadorArmado jogador = dano == True && tempoRestante > 0
                        where
                            (dano, tempoRestante) = aplicaDano jogador

-- | A função hitboxDeDano calcula a hitbox de Dano do jogador, sendo esta uma hitbox com as mesmas dimensões da hitbox do jogador posicionada à frente deste.
hitboxDeDano :: Personagem -> Hitbox
hitboxDeDano jogador | d == Este = ((x1 + larguraPersonagem, y1), (x2 + larguraPersonagem, y2))
                     | d == Oeste = ((x1 - larguraPersonagem, y1), (x2 - larguraPersonagem, x1))
                     | otherwise = ((0,0),(0,0)) -- ^ resoluçao temporária de erro Non-exhaustive patterns 
                     where
                        d = direcao jogador
                        ((x1,y1),(x2,y2)) = hitBoxDePersonagem jogador
                        (_,larguraPersonagem) = tamanho jogador

-- | A função saemDoMapaTodosInimigosMortos tira todos os inimigos mortos para fora do mapa.

saemDoMapaTodosInimigosMortos :: Mapa -> [Personagem] -> [Personagem]
saemDoMapaTodosInimigosMortos mapa = map (`saiDoMapaInimigoMorto` mapa)
-- | saemDoMapaTodosInimigosMortos mapa inimigos  = map (\i -> saiDoMapaInimigoMorto i mapa ) inimigos 

-- | Função auxiliar que verifica se o inimigo está morto.
inimigoMorto :: Personagem -> Bool
inimigoMorto inimigo = vida inimigo == 0

-- | Função saiDoMapaInimigoMorto remove o inimigo do mapa sem o remover da lista de inimigos, mudando a sua posição para não aparecer no mapa e mudando a velocidade para 0.

saiDoMapaInimigoMorto :: Personagem -> Mapa -> Personagem
saiDoMapaInimigoMorto inimigo mapa = if inimigoMorto inimigo
                                     then inimigo {posicao = (x + desapareceX ,y + desapareceY), velocidade = (0,0)}
                                     else inimigo

                                    where
                                        (x,y) = posicao inimigo
                                        desapareceX = fromIntegral (length (head matrizDeBlocos) * 2)
                                        desapareceY = fromIntegral (length matrizDeBlocos * 2 )
                                        Mapa _ _ matrizDeBlocos = mapa

-- | Função que aplica os colecionáveis ao jogador

aplicarColecionaveisAoJogador :: [(Colecionavel, Posicao)] -> Personagem -> Personagem
aplicarColecionaveisAoJogador cs jogador = foldl aplicarColecionavelAoJogador jogador (colecionaveisQueColidemComPersonagem jogador cs)

--aplicarColecionaveisAoJogador [] jogador  = jogador
--aplicarColecionaveisAoJogador (c:cs) jogador  =  aplicarColecionaveisAoJogador  cs (aplicarColecionavelAoJogador jogador c)

-- | Função que aplica o efeito do colecionável tendo em conta o seu tipo

aplicarColecionavelAoJogador :: Personagem -> (Colecionavel, Posicao) -> Personagem
aplicarColecionavelAoJogador jogador (colecionavel, _) | colecionavel == Martelo = jogador { aplicaDano = (True, 10)}
                                                       | colecionavel == Moeda = jogador { pontos = pontos jogador + 100 }

tempoDeDano :: Tempo -> Personagem -> Personagem
tempoDeDano variacaoDeTempo jogador  = if tempoRestante > 0 
                                      then jogador { aplicaDano = (True, tempoRestante - variacaoDeTempo)}
                                      else jogador { aplicaDano = (False, 0)}  

                                      where 
                                            (_, tempoRestante) = aplicaDano jogador 

-- | Função que retira da lista os colecionáveis que já colidiram com o personagem.

retirarColecionaveisQueColidemComPersonagem :: Personagem -> [(Colecionavel, Posicao)] -> [(Colecionavel, Posicao)]
retirarColecionaveisQueColidemComPersonagem personagem = filter (not . colisaoEntreHitboxes (hitBoxDePersonagem personagem) . hitboxDeColecionavel)

colecionaveisQueColidemComPersonagem :: Personagem -> [(Colecionavel, Posicao)]-> [(Colecionavel, Posicao)]
colecionaveisQueColidemComPersonagem personagem  = filter (colisaoEntreHitboxes (hitBoxDePersonagem personagem) . hitboxDeColecionavel)
--colecionaveisQueColidemComPersonagem personagem colecionaveis = filter (\c -> colisaoEntreHitboxes (hitBoxDePersonagem personagem) (hitboxDeColecionavel c) ) colecionaveis

{-|
A função hitboxDeColecionavel calcula a hitbox dos colecionáveis tendo em conta que a posição deles é no centro do objeto e o tamanho é 1x1.
-}

hitboxDeColecionavel :: (Colecionavel, Posicao) -> Hitbox
hitboxDeColecionavel (_, (x,y)) = ((x-0.5,y-0.5), (x+0.5,y+0.5))

{-|
A função hitboxDaEstrela calcula a hitbox da estrela tendo em conta que a posição é dada na posição final no mapa, é no centro do objeto e o tamanho é 1x1.
-}

hitboxDaEstrela :: Mapa -> Hitbox
hitboxDaEstrela mapa = ((x-0.5,y-0.5),(x+0.5,y+0.5))
                    where 
                        Mapa _ (x,y) _ = mapa 

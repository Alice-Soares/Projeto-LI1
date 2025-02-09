{-|
Module      : Renderização
Description : Renderização da app
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para a renderização do Menu e do Jogo.

-}

module Renderizacao where 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe (fromMaybe)
import LI12324
import DadosParaGloss
import TratarTeclas
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import DadosParaGloss 
import Graphics.Gloss (Picture)
import DadosParaGloss (InfoPic(InfoPic), EstadoDaApp (larguraJanela, alturaJanela), xParaGloss, yParaGloss)
import GHC.Real (fromIntegral)


-- | Função para renderizar o jogo de acordo com ecran atual
renderizarApp :: EstadoDaApp -> Picture
renderizarApp estado@(EstadoDaApp { ecranAtual = EcranJogo}) = renderizarJogo estado
renderizarApp estado@(EstadoDaApp { ecranAtual = EcranMenu}) = renderizarMenu estado
renderizarApp estado@(EstadoDaApp { ecranAtual = EcranGameOver}) = renderizarGameOver estado
renderizarApp estado@(EstadoDaApp { ecranAtual = EcranWin}) = renderizarWin estado
renderizarApp estado@(EstadoDaApp { ecranAtual = EcranCreditos}) = renderizarCreditos estado

-- | Função para renderizar o ecran de Game Over

renderizarGameOver :: EstadoDaApp -> Picture
renderizarGameOver estado = translate  (cantoSuperiorEsquerdoX (larguraJanela estado))  (cantoSuperiorEsquerdoY (alturaJanela estado))  $
                            translate  (fromIntegral (larguraJanela estado)/2)  (- fromIntegral (alturaJanela estado)/2)  $ gameOverPic (infoPic estado) 
        
-- | Função para renderizar o ecran de Win

renderizarWin :: EstadoDaApp -> Picture
renderizarWin estado =  translate  (cantoSuperiorEsquerdoX (larguraJanela estado))  (cantoSuperiorEsquerdoY (alturaJanela estado))  $
                        translate  (fromIntegral (larguraJanela estado)/2)  (- fromIntegral (alturaJanela estado)/2)  $ 
                        winPic (infoPic estado) 
    

renderizarCreditos :: EstadoDaApp -> Picture
renderizarCreditos estado = 
                        translate  (cantoSuperiorEsquerdoX (larguraJanela estado))  (cantoSuperiorEsquerdoY (alturaJanela estado))  $
                        translate  (fromIntegral (larguraJanela estado)/2)  (- fromIntegral (alturaJanela estado)/2)  $
                        scale (fromIntegral (larguraJanela estado) / 1417) (fromIntegral (larguraJanela estado) / 1417) $ 
                        creditosPic (infoPic estado) 

-- | Função para renderizar o menu
renderizarMenu :: EstadoDaApp -> Picture
renderizarMenu estado = let
                          items = map  ( renderizarItemMenu estado (itemSelecionado menuDaApp)  ) itemsDoMenuZipadoComPosicao 
                          titulo = translate 0  200 $ scale 3 3 $ tituloPic (menuPic estado)
                        in
                        pictures $ titulo : items 
                   -- pictures $ map  (\((i,ip),p) -> renderizarItemMenu (itemSelecionado menu) ((i,ip),p) ) itemsDoMenuZipadoComPosicao
    where
        itemsDoMenuZipadoComPosicao = zip (itens menuDaApp) [-40,(-120)..] -- ^ faz espaçamento de 100 pixeis entre os itens do menu
        menuDaApp = menu estado
    

-- | Função para renderizar um item do menu
renderizarItemMenu ::EstadoDaApp -> ItemMenu  -> ((ItemMenu,Picture),Int)  ->  Picture
renderizarItemMenu estado itemSelecionado ((menuItem,imagem),posicaoy)  = 
                                                let posicaox = 0
                                                    imagemItem = if menuItem == itemSelecionado then 
                                                               scale 5 5  $  imagem
                                                              else 
                                                               scale 4 4  $ imagem
                                                in
                                        translate (posicaox) ( fromIntegral posicaoy) $ imagemItem


-- | Função para renderizar o jogo 

renderizarJogo :: EstadoDaApp -> Picture
renderizarJogo estado =  translate  (cantoSuperiorEsquerdoX (larguraJanela estado))  (cantoSuperiorEsquerdoY (alturaJanela estado))  $
                         pictures [  renderizarMapa estado, 
                                     renderizarJogador estado, 
                                   --  rederizaAjudas estado, 
                                     renderizarInimigos estado, 
                                     renderizarColecionaveis estado,
                                     renderizarVida estado,
                                     renderizarPontos estado,
                                     renderizarEstrela estado]

-- | Função para renderizar os pontos do jogador

renderizarPontos :: EstadoDaApp -> Picture
renderizarPontos estado = let 
                            pontosDoJogador = pontos (jogador (jogo estado))
                            indiceDasUnidades = pontosDoJogador `mod` 10 
                            inidicedasDezenas = (pontosDoJogador `div` 10) `mod` 10
                            indiceDasCentenas = (pontosDoJogador `div` 100) `mod` 10
                            indiceDosMilhares = (pontosDoJogador `div` 1000) `mod` 10
                            picNumeros = numerosPic (infoPic estado)
                            largura = larguraJanela estado

                            in
                               pictures [
                                  translate  (fromIntegral largura - 60) (-50) $ scale 2 2 $ picNumeros !! indiceDasUnidades,
                                  translate  (fromIntegral largura - 100) (-50) $ scale 2 2 $ picNumeros !! inidicedasDezenas,
                                  translate  (fromIntegral largura - 140) (-50) $ scale 2 2 $ picNumeros !! indiceDasCentenas,
                                  translate  (fromIntegral largura - 180) (-50) $ scale 2 2 $ picNumeros !! indiceDosMilhares

    ]

-- | Função para renderizar as vidas do personagem 

renderizarVida :: EstadoDaApp -> Picture
renderizarVida estadoApp = let 
                            vidaDoJogador = vida (jogador (jogo estadoApp))
                            picDaVida = vidaPic  (infoPic estadoApp)
                            posicaoXDasVidas = zip [1..vidaDoJogador]  [50,90..] 
                            posicaoY = -50
                            in
                               pictures $ map (\(_,x) -> translate (x) (posicaoY) $ scale 2 2 $ picDaVida) posicaoXDasVidas

-- | Função para renderizar o mapa 
renderizarMapa :: EstadoDaApp -> Picture
renderizarMapa estado = pictures $
                        concatMap renderizaLinha (zip [0..] matrizDoJogo) -- ^ o zip dá o numero da linha e o concatMap junta as listas de pictures numa única picture 
  where
    Mapa _ _ matrizDoJogo = mapa (jogo estado)

-- | Função auxiliar que renderiza uma linha do mapa
    renderizaLinha :: (Int, [Bloco]) -> [Picture]
    renderizaLinha (linha, blocos) = map (renderizaBloco linha) (zip [0..] blocos) -- ^ o zip dá o número da coluna 

-- | Função auxiliar que renderiza um bloco
    renderizaBloco :: Int -> (Int, Bloco) -> Picture
    renderizaBloco linha (coluna, bloco) =
      translate  (fromIntegral coluna * realToFrac pixeisPorBloco + (fromIntegral pixeisPorBloco/2)) (fromIntegral linha * realToFrac(-pixeisPorBloco) - (fromIntegral pixeisPorBloco/2)) $
        case bloco of
          Vazio ->  blocoVazio (blocos estado)
          Plataforma -> blocoPlataforma (blocos estado)
          Escada -> blocoEscada (blocos estado)
          Alcapao -> blocoAlcapao (blocos estado)


-- Função para renderizar os inimigos
renderizarInimigos :: EstadoDaApp -> Picture
renderizarInimigos estadoApp = let 
                                picDoFantasma = fantasmaPic (personagensPic estadoApp)
                                picDoMacacoMalvado = macacoMalvadoPic (personagensPic estadoApp)
                                inimigosDoJogo = inimigos $ jogo estadoApp
                                tempo = tempoTotal estadoApp
                                listaDePicsInimigos = map (inimigosParaPic picDoFantasma picDoMacacoMalvado (realToFrac tempo)) inimigosDoJogo
                                in
                                    pictures listaDePicsInimigos

-- | Função auxiliar que renderiza um inimigo aplicando-lhe uma animação
inimigosParaPic ::  [Picture] -> [Picture] -> Tempo -> Personagem -> Picture
inimigosParaPic  picFantasma picMacaco tempo inimigo  | tipo inimigo == Fantasma = 
                                                            case  direcaoFantasma of
                                                                Este -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                                                   scale (-2) 2 $ 
                                                                                   picFantasma !! indiceAnimacao1 
                                                                Oeste -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                                                   scale 2 2 $ 
                                                                                   picFantasma !! indiceAnimacao1 
                                                      | tipo inimigo == MacacoMalvado = translate (xParaGloss x) (yParaGloss y+15)  $ 
                                                                                        scale 2 2 $ 
                                                                                        picMacaco !! indiceAnimacao2
                                              where  
                                                    (x,y) = posicao inimigo
                                                    indiceAnimacao1 = floor (tempo / tempoPorFrame) `mod` numPicsAnimacao1
                                                    indiceAnimacao2 = floor (tempo / tempoPorFrame) `mod` numPicsAnimacao2
                                                    tempoPorFrame = 0.1
                                                    numPicsAnimacao1 = length picFantasma
                                                    numPicsAnimacao2 = length picMacaco
                                                    direcaoFantasma = direcao inimigo

{-| Função que renderiza o jogador e aplica uma animação ao mudar a picture do jogador usada com o passar do tempo. 
-}

renderizarJogador :: EstadoDaApp -> Picture
renderizarJogador estadoApp = let
                                (x,y) = posicao (jogador (jogo estadoApp))
                                picDoJogadorAndar = jogadorAndarPic (personagensPic estadoApp)
                                picDoJogadorSubir = jogadorSubirPic (personagensPic estadoApp)
                                picDoJogadorArmado = jogadorArmadoPic (personagensPic estadoApp)
                                numPicsAnimacao1 = length picDoJogadorAndar
                                numPicsAnimacao2 = length picDoJogadorSubir
                                numPicsAnimacao3 = length picDoJogadorArmado
                                tempoPorFrame = 0.1
                                (velocidadeX, velocidadeY) = velocidade ((jogador (jogo estadoApp)))
                                tempoTotalDaApp = tempoTotal estadoApp
                                indiceAnimacao1 = if velocidadeX == 0 then 0
                                                  else floor (tempoTotalDaApp / tempoPorFrame) `mod` numPicsAnimacao1
                                indiceAnimacao2 = if velocidadeY == 0 then 0
                                                  else floor (tempoTotalDaApp / tempoPorFrame) `mod` numPicsAnimacao2
                                indiceAnimacao3 = floor (tempoTotalDaApp / tempoPorFrame) `mod` numPicsAnimacao3
                                direcaoDoJogador = direcao (jogador (jogo estadoApp))
                                (jogadorArmado, _) = aplicaDano (jogador (jogo estadoApp))
                                in
                                    case jogadorArmado of 
                                        True ->  case direcaoDoJogador of
                                                    Este -> translate  (xParaGloss x) (yParaGloss (y-0.4)) $ 
                                                            scale (-0.5) 0.5 $ 
                                                            picDoJogadorArmado !! indiceAnimacao3
                                                    Oeste -> translate  (xParaGloss x) (yParaGloss (y-0.4)) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorArmado !! indiceAnimacao3
                                                    Norte -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorSubir !! indiceAnimacao2
                                                    Sul   -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorSubir !! indiceAnimacao2
                                                    _ -> blank


                                        False -> case direcaoDoJogador of
                                                    Este -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale (-0.5) 0.5 $ 
                                                            picDoJogadorAndar !! indiceAnimacao1
                                                    Oeste -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorAndar !! indiceAnimacao1
                                                    Norte -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorSubir !! indiceAnimacao2
                                                    Sul   -> translate  (xParaGloss x) (yParaGloss y) $ 
                                                            scale 0.5 0.5 $ 
                                                            picDoJogadorSubir !! indiceAnimacao2
                                                    _ -> blank

                                    
-- | Função que renderiza os colecionáveis 
                        
renderizarColecionaveis :: EstadoDaApp -> Picture
renderizarColecionaveis estadoApp = let 
                                        listaColecionaveis = colecionaveis (jogo estadoApp)

                                        in 
                                           pictures $ map (\c -> renderizarUmColecionavel estadoApp c) listaColecionaveis
                                 
-- | Função que renderiza um colecionável tendo em conta o seu tipo

renderizarUmColecionavel :: EstadoDaApp -> (Colecionavel, Posicao) -> Picture
renderizarUmColecionavel estadoApp (tipoColecionavel, (x,y)) = let  
                                                               picDoMartelo = marteloPic (colecionaveisPic estadoApp)
                                                               picDaMoeda = moedaPic (colecionaveisPic estadoApp)
                                                     

                                                                in
                                                                    case tipoColecionavel of 
                                                                        Martelo -> translate (xParaGloss x) (yParaGloss y) $ 
                                                                            scale 2 2 $ picDoMartelo
                                                                        Moeda -> translate (xParaGloss x) (yParaGloss y) $ 
                                                                            scale 0.1 0.1 $ picDaMoeda
                                                                        _       -> blank

-- | Função que renderiza a estrela

renderizarEstrela :: EstadoDaApp -> Picture
renderizarEstrela estadoApp = let
                                 picDaEstrela = estrelaPic (infoPic estadoApp)
                                 Mapa _ (x,y) _ = mapa (jogo estadoApp)
                               in 
                                translate (xParaGloss x) (yParaGloss y) $ 
                                scale 2 2 $ picDaEstrela

{-| 
   
-- | Função para renderizar as ajudas, ou seja, informações que ajudam enquanto se tentam resolver problemas de código.


rederizaAjudas :: EstadoDaApp -> Picture
rederizaAjudas estadoApp = 
                let 
                    ((xi,yi),(xs,ys)) = hitBoxDePersonagem (jogador (jogo estadoApp))
                    (x,y) = posicao (jogador (jogo estadoApp))

                in 
                    pictures [ 
                                translate (xParaGloss x) (yParaGloss y) $ hitBoxpic (hitBoxDePersonagem (jogador (jogo estadoApp))),
                                translate (xParaGloss 5 ) (yParaGloss 5) $ ajudaDeJogadorPic (jogador (jogo estadoApp))
                             ]

hitBoxpic :: Hitbox -> Picture
hitBoxpic ((xi,yi),(xs,ys)) = color red $ rectangleWire (realToFrac ((xs* fromIntegral pixeisPorBloco)-(xi* fromIntegral pixeisPorBloco))) (realToFrac ((ys* fromIntegral pixeisPorBloco)-(yi* fromIntegral pixeisPorBloco)))
                            

ajudaDeJogadorPic :: Personagem -> Picture
ajudaDeJogadorPic personagem =  color blue $ textoPic (textoDePersonagem personagem)

textoPic :: [String] -> Picture
textoPic linhas =   pictures $ zipWith (\y linha -> scale 0.1 0.1 $ translate 0 y $ text linha) [0,-120..] linhas

textoDePersonagem :: Personagem -> [String]
textoDePersonagem personagem =
    [ 
        "Vida: " ++ show (vida personagem),
        "Pontos: " ++ show (pontos personagem),
        "Aplica Dano: " ++ show (aplicaDano personagem),
        "Direcao: " ++ show (direcao personagem),
        "Em Escada: " ++ show (emEscada personagem),
        "Ressalta: " ++ show (ressalta personagem),
        "Posicao: " ++ show (posicao personagem),
        "Tamanho: " ++ show (tamanho personagem),
        "Tipo: " ++ show (tipo personagem),
         "Velocidade: " ++ show (velocidade personagem),
        "x mod 32: " ++ show (floor (fst (posicao personagem)) `mod` 32),
        "hitbox: " ++ show (hitBoxDePersonagem personagem)
    ]

-}


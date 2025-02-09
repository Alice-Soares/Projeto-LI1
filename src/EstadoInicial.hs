{-|
Module      : EstadoInicial
Description : Cria um estado inicial
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para criar o estado inicial e mudar parâmetros.

-}

module EstadoInicial where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe (fromMaybe)
import DadosParaGloss
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import LI12324
import Graphics.Gloss
import DadosParaGloss (EstadoDaApp)

-- | Função para criar o estado inicial da aplicação

criaEstadoInicial :: IO EstadoDaApp
criaEstadoInicial = do
            blocos <- criaBlocosPicInicial
            personagensPic <- criaPersonagensPicInicial
            colecionaveisPic <- criaColecionaveisPicInicial
            niveisDoJogo <- mapM carregaMatrizDeCaracteres ["mapas/mapa1.txt","mapas/mapa2.txt"]
            infoPic <- criaInfoPicInicial
            menuPics <- criaMenuPics
            let indiceNivelAtual = 0
            let larguraJanela = length (head (niveisDoJogo !! indiceNivelAtual) ) * pixeisPorBloco
            let alturaJanela = length ( (niveisDoJogo !! indiceNivelAtual)) * pixeisPorBloco
            let menuItens = zip ([minBound .. maxBound] :: [ItemMenu]) (botoesPic menuPics)
            return EstadoDaApp {
                        tempoTotal = 0,
                        ecranAtual = EcranMenu,
                        menu = Menu { itens = menuItens , itemSelecionado = IniciarJogo1 },
                         blocos = blocos,
                         niveis = niveisDoJogo,
                         nivelAtual = indiceNivelAtual,
                         personagensPic = personagensPic,
                         infoPic = infoPic,
                         menuPic = menuPics,
                         colecionaveisPic = colecionaveisPic,
                         larguraJanela = larguraJanela,
                         alturaJanela = alturaJanela
            }

-- | Função que inicializa o jogo tendo em conta o nível selecionado no estado da app.

reiniciaJogoComNivelAtual :: EstadoDaApp -> EstadoDaApp
reiniciaJogoComNivelAtual estado = estado {jogo = Jogo {
                                            mapa = mapaNivel,
                                            inimigos = criaInimigosInicial nivelAtualizado ,
                                            colecionaveis = criaColecionaveisInicial nivelAtualizado,
                                            jogador = criaJogadorInicial mapaNivel
                                          }
                              } 
                               where mapaNivel = criaMapaDoNivel nivelAtualizado
                                     nivelAtualizado = (niveis estado) !! (nivelAtual estado)

-- | Função que carrega as imagens dos colecionáveis

criaColecionaveisPicInicial :: IO ColecionaveisPic
criaColecionaveisPicInicial = do
    maybeMarteloPic <- loadJuicy "sprites/martelo.bmp"
    maybeMoedaPic <- loadJuicy "sprites/moeda.png"
    let defaultPic = color red $ circle 2
    let marteloPic = fromMaybe defaultPic maybeMarteloPic
    let moedaPic = fromMaybe defaultPic maybeMoedaPic 
    let colecionaveis = ColecionaveisPic { marteloPic = marteloPic, moedaPic = moedaPic}
    return colecionaveis

-- | Função que carrega as imagens das informações

criaInfoPicInicial :: IO InfoPic
criaInfoPicInicial = do
    maybeGameOverPic <- loadJuicy "sprites/gameOver.png"
    maybeWinPic <- loadJuicy "sprites/win.png"
    maybeVidaPic <- loadJuicy "sprites/vida.bmp"
    maybeNumerosPic <- mapM loadJuicy ["sprites/zero.bmp", "sprites/um.bmp", "sprites/dois.bmp", "sprites/tres.bmp", "sprites/quatro.bmp", "sprites/cinco.bmp", "sprites/seis.bmp", "sprites/sete.bmp", "sprites/oito.bmp", "sprites/nove.bmp" ]
    maybeEstrelaPic <- loadJuicy "sprites/estrela.bmp"
    maybeCreditosPic <- loadJuicy "sprites/creditos.png"

    let defaultPic = color red $ circleSolid 3
    let gameOverPic = fromMaybe defaultPic maybeGameOverPic
    let winPic = fromMaybe defaultPic maybeWinPic
    let vidaPic = fromMaybe defaultPic maybeVidaPic
    let numerosPic = map (fromMaybe defaultPic) maybeNumerosPic
    let estrelaPic = fromMaybe defaultPic maybeEstrelaPic
    let creditosPic = fromMaybe defaultPic maybeCreditosPic
    let infoPic = InfoPic gameOverPic winPic vidaPic numerosPic estrelaPic creditosPic

    return infoPic

-- | Função que carrega as imagens dos blocos

criaBlocosPicInicial :: IO BlocosPic
criaBlocosPicInicial = do
    maybeBlocoPlataformaPic <- loadJuicy "sprites/bloco-plataforma.bmp"
    maybeBlocoVazioPic <- loadJuicy "sprites/bloco-vazio.png"
    maybeBlocoEscadaPic <- loadJuicy "sprites/bloco-escada.bmp"
    maybeBlocoAlcapaoPic <- loadJuicy "sprites/bloco-alcapao.bmp"

    let defaultPic = blank

    let blocoPlataformaPic = scale 4 4 $  fromMaybe defaultPic maybeBlocoPlataformaPic
    let blocoVazioPic = scale 4 4 $ fromMaybe defaultPic maybeBlocoVazioPic
    let blocoEscadaPic = scale 4 4 $ fromMaybe defaultPic maybeBlocoEscadaPic
    let blocoAlcapaoPic = scale 4 4 $ fromMaybe defaultPic maybeBlocoAlcapaoPic

    let blocos = BlocosPic blocoPlataformaPic blocoVazioPic blocoEscadaPic blocoAlcapaoPic
    return blocos

-- | Função que carrega as imagens dos personagens

criaPersonagensPicInicial :: IO PersonagensPic
criaPersonagensPicInicial = do
    maybeJogadorAndarPics <- mapM loadJuicy ["sprites/mario-andar1.bmp","sprites/mario-andar2.bmp","sprites/mario-andar3.bmp"]
    maybeJogadorSubirPics <- mapM loadJuicy ["sprites/mario-subir1.bmp","sprites/mario-subir2.bmp","sprites/mario-subir3.bmp", "sprites/mario-subir4.bmp"]
    maybeJogadorArmadoPics <- mapM loadJuicy ["sprites/mario-martelo1.bmp", "sprites/mario-martelo2.bmp", "sprites/mario-martelo3.bmp", "sprites/mario-martelo4.bmp", "sprites/mario-martelo5.bmp","sprites/mario-martelo6.bmp"]
    maybeFantasmaPics <- mapM loadJuicy ["sprites/fantasma1.bmp", "sprites/fantasma2.bmp", "sprites/fantasma3.bmp"]
    maybeMacacoMalvadoPics <- mapM loadJuicy ["sprites/macacoMalvado1.bmp","sprites/macacoMalvado2.bmp","sprites/macacoMalvado3.bmp"]
    let defaultPic = color red $ circleSolid 23
    let jogadorAndarPics = map (scale 4 4) $ map  (fromMaybe defaultPic) maybeJogadorAndarPics
    let jogadorSubirPics = map (scale 4 4) $ map  (fromMaybe defaultPic) maybeJogadorSubirPics
    let jogadorArmadoPics = map (scale 4 4) $ map (fromMaybe defaultPic) maybeJogadorArmadoPics
    let fantasmaPics =  map (fromMaybe defaultPic) maybeFantasmaPics
    let macacoMalvadoPics =  map (fromMaybe defaultPic) maybeMacacoMalvadoPics
    let personagensPic = PersonagensPic {
                                            jogadorAndarPic = jogadorAndarPics,
                                            jogadorSubirPic = jogadorSubirPics,
                                            jogadorArmadoPic = jogadorArmadoPics, 
                                            fantasmaPic = fantasmaPics,
                                            macacoMalvadoPic = macacoMalvadoPics}
    return personagensPic

-- | Função que carrega as imagens dos botões do menu

criaMenuPics :: IO MenuPic
criaMenuPics = do    
    maybeBotoesPic   <- mapM loadJuicy ["sprites/botaoJogo1.png","sprites/botaojogo2.png", "sprites/botaocreditos.png","sprites/botaosair.png" ] -- ^Imagens carregadas pela mesma ordem do menu
    maybeTituloPic   <- loadJuicy "sprites/titulo.png"
    let defaultPic = color red $ circleSolid 23
    let botoesPics    = map (fromMaybe defaultPic) maybeBotoesPic
    let tituloPics      = fromMaybe defaultPic maybeTituloPic
    let menuPic = MenuPic {
        botoesPic = botoesPics,
        tituloPic = tituloPics
    }

    return menuPic


{-| Usamos um ficheiro de texto para criar os mapas por vários motivos, nomeadamente por ser mais fácil posicionar os inimigos, jogador, etc. ,
mas principalmente porque seria uma espécie de editor de mapas que nos permite fazer vários mapas a partir de ficheiros de texto.

-}

carregaMatrizDeCaracteres :: FilePath -> IO Nivel
carregaMatrizDeCaracteres nomeDoFicheiro = do
    texto <- readFile nomeDoFicheiro
    let matrizDeCaractares = textoParaMatrizDeCaractares texto
    return matrizDeCaractares

textoParaMatrizDeCaractares :: String -> [[Char]]
textoParaMatrizDeCaractares conteudo = map (filter (/= ' ')) (lines conteudo) -- ^ transforma o texto numa matriz de caracteres, retirando os espaços

-- | Funções auxiliares que tranformam a matriz de caracteres numa matriz de Blocos, criando a matriz do mapa.

criaMatrizDeBlocos :: [[Char]] -> [[Bloco]]
criaMatrizDeBlocos = map (map charParaBloco)

charParaBloco :: Char -> Bloco
charParaBloco 'P' = Plataforma
charParaBloco 'E' = Escada
charParaBloco 'V' = Vazio
charParaBloco 'A' = Alcapao
charParaBloco _ = Vazio

-- | Função para criar o mapa inicial
criaMapaDoNivel :: Nivel -> Mapa
criaMapaDoNivel  matriz  = let
                                matrizDeBlocos = criaMatrizDeBlocos matriz
                                (y,x,c) = head (filter (\(x,y,c) -> c == 'J') (ziparMatrizdeCharComIndices matriz)) -- ^ Vai buscar a posição inicial do personagem definida no mapa
                                (yf,xf,ch) = head (filter (\(x,y,ch) -> ch == 'W') (ziparMatrizdeCharComIndices matriz)) -- ^ Vai buscar a posição final 
                            in
                                 Mapa ((x ,y),Este) (xf,yf) matrizDeBlocos

-- | Função auxiliar que faz zip da matriz com índices para obter as posições dos blocos

ziparMatrizdeCharComIndices ::[[Char]] -> [(Double,Double,Char)]
ziparMatrizdeCharComIndices matriz =  concat (zipWith zipLinha [0..] matriz)
    where
        zipLinha indiceLinha = zipWith (zipBloco indiceLinha) [0..]
        zipBloco indiceLinha indiceColuna bloco = (indiceLinha,indiceColuna,bloco)


-- | Função para criar o jogador inicial
criaJogadorInicial :: Mapa -> Personagem
criaJogadorInicial mapa = let
                Mapa ((posicaoInicialX,posicaoInicialY),direcao) _ _ = mapa
                in
                Personagem {
                velocidade = (0,0),
                tipo = Jogador,
                posicao = (posicaoInicialX,posicaoInicialY),
                direcao = direcao,
                tamanho = (0.9,0.9),
                emEscada = False,
                ressalta = False,
                vida = 3,
                pontos = 0,
                aplicaDano = (False,0)
            }

-- | Função para criar os inimigos iniciais
criaInimigosInicial :: [[Char]] -> [Personagem]
criaInimigosInicial matriz = criaMacacoMalvado matriz : criaFantasmas matriz


-- | Função que cria o macaco malvado

criaMacacoMalvado ::[[Char]] ->  Personagem
criaMacacoMalvado matrizDeCaracteres = let
                                        (y,x,_) =head (filter (\(_,_,c) -> c == 'K') (ziparMatrizdeCharComIndices matrizDeCaracteres))-- mudar a posição
                                        in
                                            Personagem {
                                                        velocidade = (0,0),
                                                        tipo = MacacoMalvado,
                                                        posicao = (x,y),
                                                        direcao = Este,
                                                        tamanho = (1,1),
                                                        emEscada = False,
                                                        ressalta = True,
                                                        vida = 1,
                                                        pontos = 0,
                                                        aplicaDano = (True,0)
                                                    }


-- | Função que cria uma lista de fantasmas
criaFantasmas :: [[Char]] -> [Personagem]
criaFantasmas matrizDeCaracteres = let
                                listaDeFantasmas =  (filter (\(_,_,c) -> c == 'F') (ziparMatrizdeCharComIndices matrizDeCaracteres)) -- ^ filtra as posicoes dos fantasmas da lista de caracteres
                                 in
                                map criaUmFantasma listaDeFantasmas

-- | Função que cria um fantasma

criaUmFantasma :: (Double,Double,Char) -> Personagem
criaUmFantasma (y,x,_) =   Personagem {
                                velocidade = (2,0),
                                tipo = Fantasma,
                                posicao = (x+0.5, y+0.49), -- ^ y aumentado em 0.49 para não colidir com a plataforma
                                direcao = Este,
                                tamanho = (1,1),
                                emEscada = False,
                                ressalta = True,
                                vida = 1,
                                pontos = 0,
                                aplicaDano = (True,0)
                            }


-- Função para criar os colecionaveis iniciais
criaColecionaveisInicial :: [[Char]] -> [(Colecionavel,Posicao)]
criaColecionaveisInicial matrizDeCaracteres = let                                    
                                                listaDeMartelos = (filter (\(_,_,c) -> c == 'M') (ziparMatrizdeCharComIndices matrizDeCaracteres))
                                                listaDeMoedas = (filter (\(_,_,c) -> c == 'O') (ziparMatrizdeCharComIndices matrizDeCaracteres))
                                              in
                                                map criaUmColecionavel ( listaDeMoedas ++ listaDeMartelos)

-- | Função auxiliar que cria um colecionável

criaUmColecionavel :: (Double, Double, Char) ->  (Colecionavel,Posicao)
criaUmColecionavel (y,x, c) | c == 'M' = (Martelo, ( x+0.5,y+0.5))
                            | otherwise = (Moeda, ( x+0.5,y+0.5))





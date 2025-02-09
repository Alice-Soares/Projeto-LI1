module TestesTarefa4 (testesTarefa4) where

import LI12324
import Tarefa4 (atualiza)
import Test.HUnit

mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 7),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorParado
    }

teste01 :: Test
teste01 = "T01: Quando não há nenhuma acção, o jogo permanece inalterado" ~: jogo01 ~=? atualiza [Nothing] Nothing jogo01

andarDireita01 :: Jogo
andarDireita01 = atualiza [Nothing] (Just AndarDireita) jogo01

teste02 :: Test
teste02 = TestLabel "T02" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarDireita, o vetor velocidade do jogador é positivo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) > 0
    testeB = "B: Quando a acção é AndarDireita, a orientação do jogador é Este" ~: Este ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = atualiza [Nothing] (Just AndarDireita) jogo01

teste03 :: Test
teste03 = TestLabel "T03" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é AndarEsquerda, o vetor velocidade do jogador é negativo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) < 0
    testeB = "B: Quando a acção é AndarEsquerda, a orientação do jogador é Oeste" ~: Oeste ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = atualiza [Nothing] (Just AndarEsquerda) jogo01

teste04 :: Test
teste04 = TestLabel "T04" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Saltar, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSaltar) < 0
    testeB = "B: Quando a acção é Saltar, a orientação do jogador não muda" ~: (direcao . jogador $ jogo01) ~=? (direcao . jogador $ resultadoSaltar)
    resultadoSaltar = atualiza [Nothing] (Just Saltar) jogo01

jogadorEmFrenteEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7.5, 7),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo02 :: Jogo
jogo02 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorEmFrenteEscada
    }

--
teste05 :: Test
teste05 = TestLabel "T05" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Subir, o vetor velocidade do jogador é negativo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSubir) < 0
    testeB = "B: Quando a acção é Saltar, o jogador passa a estar em escada" ~: True ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = atualiza [Nothing] (Just Subir) jogo01 -- ^  o teste deveria ser para um jogador em escada, talvez ojogo 02
 
jogadorEmEscada =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (7, 7),
      direcao = Norte,
      tamanho = (0.8, 0.8),
      emEscada = True,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

teste06 :: Test
teste06 = TestLabel "T06" $ test [testeA, testeB]
  where
    testeA = "A: Quando a acção é Descer, o vetor velocidade do jogador é positivo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSubir) > 0
    testeB = "B: Quando a acção é Descer, o jogador continua em escada" ~: (emEscada jogadorEmEscada) ~=? (emEscada . jogador $ resultadoSubir)
    resultadoSubir = atualiza [Nothing] (Just Descer) jogo01

testesTarefa4 :: Test
testesTarefa4 = TestLabel "Tarefa4 (atualiza)" $ test [teste01, teste02, teste03, teste04, teste05, teste06]


  
  mapa02 :: Mapa
mapa02 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio],
      [Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio],
      [Vazio, Alcapao, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

jogadorAndandoDireita =
  Personagem
    { velocidade = (1.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 7),
      direcao = Este,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogo02 :: Jogo
jogo02 =
  Jogo
    { mapa = mapa02,
      inimigos = [inimigoParado],
      colecionaveis = [],
      jogador = jogadorAndandoDireita
    }

-- Teste adicional quando não há ação
teste07 :: Test 
teste07 = TestLabel "T07: Não há qualquer ação no jogo, logo fica inalterado" $ jogo ~=? atualiza [Nothing] Nothing jogo01

-- Teste adicional para o jogador quando anda para a esquerda
teste08 :: Test
teste08 = TestLabel "T08" $ test [testeA, testeB]
  where
    testeA = "A: Quando a ação é AndarEsquerda, o vetor velocidade do jogador é negativo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarEsquerda) < 0
    testeB = "B: Quando a ação é AndarEsquerda, a orientação do jogador é Oeste" ~: Oeste ~=? (direcao . jogador $ resultadoAndarEsquerda)
    resultadoAndarEsquerda = atualiza [Nothing] (Just AndarEsquerda) jogo01

-- Teste adicional para o jogador quando anda para a direita 
teste09 :: Test
teste09 = TestLabel "T09" $ test [testeA, testeB]
  where
    testeA = "A: Quando a ação é AndarDireita, o vetor velocidade do jogador é negativo na componente do X" ~: True ~=? (fst . velocidade . jogador $ resultadoAndarDireita) < 0
    testeB = "B: Quando a ação é AndarDireita, a orientação do jogador é Oeste" ~: Oeste ~=? (direcao . jogador $ resultadoAndarDireita)
    resultadoAndarDireita = atualiza [Nothing] (Just AndarDireita) jogo01


-- Teste adicional quando desce
teste10 :: Test
teste10 = TestLabel "T10" $ test [testeA, testeB]
  where
    testeA = "A: Quando a ação é Descer, o vetor velocidade do jogador é positivo na componente do Y" ~: True ~=? (snd . velocidade . jogador $ resultadoDescer) > 0
    testeB = "B: Quando a ação é Descer, o jogador continua em escada" ~: (emEscada jogadorEmEscada) ~=? (emEscada . jogador $ resultadoDescer)
    resultadoDescer = atualiza [Nothing] (Just Descer) jogo01

-- Teste adicional quando salta 
teste11 :: Test 
teste11 = TestLabel "T11" $ test [testeA, testeB]
  where 
    testeA = "A: Quando o jogador salta, então a velocidade é negativa na componente Y" ~: True ~=? (snd . velocidade . jogador $ resultadoSaltar)
    testeB = "B: Quando o jogador salta, não muda de orientação" ~=? (direcao . jogador $ jogo01) ~=? (direcao . jogador $ resultadoSaltar)
    resultadoSaltar = atualiza [Nothing] (Just Saltar) jogo01
    


testesTarefa4 :: Test
testesTarefa4 = TestLabel "Tarefa4 (atualiza)" $ test [teste01, teste02, teste03, teste04, teste05, teste06, teste07, teste08, teste09, teste10, teste11]


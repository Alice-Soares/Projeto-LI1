import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe (fromMaybe)
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import Text.ParserCombinators.ReadP (char)
import DadosParaGloss
import Renderizacao
import TratarTeclas
import EstadoInicial
import DadosParaGloss (EstadoDaApp(tempoTotal))



main :: IO ()
main = do
          estadoInicial <- criaEstadoInicial
          play
            (InWindow "Donkey Kong" (fromIntegral (larguraJanela estadoInicial) , fromIntegral ( alturaJanela estadoInicial)) (posicaoXInicialDajanela, posicaoYInicialDajanela))  -- Configurações da janela
            black                                          -- ^ Cor de fundo
            60                                             -- ^ Frames por segundo
            estadoInicial                                  -- ^ Estado inicial
            renderizarApp                                  -- ^ Função de renderização
            tratarTeclas                                   -- ^ Tratamento de eventos
            atualizarJogo                                  -- ^ Atualização do estado



-- | Função chamada para atualizar o estado do jogo.

atualizarJogo :: Float -> EstadoDaApp -> EstadoDaApp
atualizarJogo variacaoEmSegundos estado@(EstadoDaApp { ecranAtual = EcranJogo}) = estado {
    jogo = jogoAtualizado,
    tempoTotal = tempoTotalAtualizado,
    ecranAtual = ecranAtualizado
    }

    where
        jogadorDoJogo = jogador (jogo estado)
        mapaJogo = mapa (jogo estado)
        hitboxJogador = hitBoxDePersonagem jogadorDoJogo
        hitboxEstrela = hitboxDaEstrela mapaJogo
        jogoAtualizado = movimenta (floor tempoTotalAtualizado) (realToFrac variacaoEmSegundos) (jogo estado)
        tempoTotalAtualizado = tempoTotal estado + variacaoEmSegundos -- ^ Acumula o tempo passado, calculando o tempo total de jogo
        ecranAtualizado
          | vida jogadorDoJogo == 0 = EcranGameOver -- ^ muda o ecran quando o jogador perde
          | colisaoEntreHitboxes hitboxJogador hitboxEstrela = EcranWin -- ^ muda o ecran quando o jogador ganha
          | otherwise = EcranJogo
atualizarJogo _ estado = estado

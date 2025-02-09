{-|
Module      : TratarTeclas
Description : Define tipos de dados usados no Gloss.
Copyright   : Alice Isabel Faria Soares <a106804@alunos.uminho.pt>
              Ana Sofia Martins Vasconcelos <a106794@alunos.uminho.pt>

Módulo para o tratamento de teclas

-}

module TratarTeclas where 
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Juicy (loadJuicy)
import Data.Maybe (fromMaybe)
import DadosParaGloss
import LI12324
import Tarefa4
import EstadoInicial



-- | Função para tratar eventos de teclado 
tratarTeclas :: Event -> EstadoDaApp -> EstadoDaApp
tratarTeclas e estado@(EstadoDaApp { ecranAtual = EcranJogo}) = trataTeclasEmJogo e estado 
tratarTeclas e estado@(EstadoDaApp { ecranAtual = EcranMenu}) = trataTeclasEmMenu e estado
tratarTeclas e estado@(EstadoDaApp { ecranAtual = EcranGameOver}) = trataTeclasFimDeJogo e estado
tratarTeclas e estado@(EstadoDaApp { ecranAtual = EcranWin}) = trataTeclasFimDeJogo e estado
tratarTeclas e estado@(EstadoDaApp { ecranAtual = EcranCreditos}) = trataTeclasFimDeJogo e estado

trataTeclasFimDeJogo :: Event -> EstadoDaApp -> EstadoDaApp
trataTeclasFimDeJogo evento estado =
     case evento of
        (EventKey (SpecialKey KeyDelete) Down _ _) -> estado { ecranAtual = EcranMenu} 
                                           
        _ -> estado 

-- | Função que trata as teclas em Jogo
trataTeclasEmJogo :: Event -> EstadoDaApp -> EstadoDaApp
trataTeclasEmJogo evento estado =
    case evento of

        (EventKey (SpecialKey tecla) Down _ _) ->
            case tecla of
                KeyLeft -> estado { jogo = atualiza [] (Just AndarEsquerda) (jogo estado)}
                KeyRight -> estado { jogo = atualiza [] (Just AndarDireita) (jogo estado)}
                KeyUp -> estado { jogo = atualiza [] (Just Subir) (jogo estado)}
                KeyDown -> estado { jogo = atualiza [] (Just Descer) (jogo estado)}
                KeySpace -> estado { jogo = atualiza [] (Just Saltar) (jogo estado)}
                KeyEnter -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyDelete -> estado { ecranAtual = EcranMenu}
                _ -> estado
        (EventKey (SpecialKey tecla) Up _ _) ->
            case tecla of
                KeyRight -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyLeft -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyUp -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                KeyDown -> estado { jogo = atualiza [] (Just Parar) (jogo estado)}
                _ -> estado
        _ -> estado 

-- | Função que trata as teclas em Menu
trataTeclasEmMenu :: Event -> EstadoDaApp -> EstadoDaApp
trataTeclasEmMenu evento estado =
    case evento of
        (EventKey (SpecialKey KeyDown) Down _ _) -> 
            estado { menu = (menu estado) {itemSelecionado = menuAtualizadoDown }}
            where menuAtualizadoDown = itemSeguinte (itemSelecionado (menu estado))

        (EventKey (SpecialKey KeyUp) Down _ _) -> 
            estado { menu = (menu estado) {itemSelecionado = menuAtualizadoUp }}
            where menuAtualizadoUp = itemAnterior (itemSelecionado (menu estado))

        (EventKey (SpecialKey KeyEnter) Down _ _) -> 
            trataKeyInsertEmMenu (itemSelecionado (menu estado)) estado

        _ -> estado  

-- | Função auxiliar que trata a tecla insert no Menu tendo em conta o item selecionado.

trataKeyInsertEmMenu :: ItemMenu -> EstadoDaApp -> EstadoDaApp
trataKeyInsertEmMenu Sair _ = error "Sair"
trataKeyInsertEmMenu IniciarJogo1 estado = reiniciaJogoComNivelAtual $   estado { ecranAtual = EcranJogo, nivelAtual = 0 }
trataKeyInsertEmMenu IniciarJogo2 estado = reiniciaJogoComNivelAtual $   estado { ecranAtual = EcranJogo, nivelAtual = 1 }
trataKeyInsertEmMenu Creditos estado = reiniciaJogoComNivelAtual $   estado { ecranAtual = EcranCreditos}
trataKeyInsertEmMenu _  estado = estado


{-| Funções auxiliares que devolvem:

* o item seguinte ao selecionado no menu e se este for o último volta ao primeiro

* o item anterior ao selecionado no menu e se este for o primeiro volta ao último
-}

itemSeguinte :: ItemMenu -> ItemMenu
itemSeguinte item | item == maxBound = minBound
                  | otherwise        = succ item

itemAnterior :: ItemMenu -> ItemMenu
itemAnterior item | item == minBound = maxBound
                  | otherwise        = pred item
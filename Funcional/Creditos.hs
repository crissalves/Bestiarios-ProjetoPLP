module Creditos where

import qualified Screen as Scr
import qualified Terminal
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Exit
import Control.Concurrent

texto :: [[Char]]
texto = [
    "Projeto para disciplina Paradigmas de Linguagens de Programação. Ministrada por Everton Leandro.", 
    "  ", 
    "Código escito por: ", 
    " ",
    " Cristian Alves da Silva.",
    " José Erik Dionísio da Silva.",
    " ",
    "  ",
    "Com o auxílio de: ",
    " ",
    " Roberta Felix.",
    " ",
    " Data de entrega: 17/07/2022."]

menu :: [[Char]]
menu = [
    "Voltar para o menu."]

commandsTable :: [[Char]]
commandsTable = [ 
    " f - selecionar        "]

-- cria o menu e imprime ele na tela
printMenu :: [[Char]] -> IO()
printMenu menuTab = do
    let initialBuffer = Scr.createScreenBuffer Scr.width Scr.height Scr.emptyPxl
    let textoBuf = Scr.createBufferFromStringMatrix texto
    let menuBuf = Scr.createBufferFromStringMatrix menuTab
    let tableBuf = Scr.createBufferFromStringMatrix commandsTable

    let tmp1 = Scr.renderCentralized initialBuffer textoBuf 0 1
    let tmp2 = Scr.renderCentralized tmp1 menuBuf 0 10
    let tmp3 = Scr.renderInBuffer tmp2 tableBuf 1 3
  

    Scr.printScreen tmp3

mainLoop :: Int -> IO()
mainLoop index = do

    printMenu $ menu 

    -- pega um unico caracter da entrada
    hSetBuffering stdin NoBuffering
    command <- getChar
    hSetBuffering stdin LineBuffering

    -- processa o comando recebido
    case command of 'f' -> putStrLn " "
                    cmd -> mainLoop 0
 


main :: IO()
main = mainLoop 0
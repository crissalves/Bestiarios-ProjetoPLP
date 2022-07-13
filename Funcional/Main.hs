import qualified Screen as Scr
import qualified Terminal
import qualified MenuCriatura as Mcrt 
import qualified Creditos as Crdt
import System.IO
import System.IO.Unsafe (unsafeDupablePerformIO)
import System.Exit
import Control.Concurrent


title :: [[Char]]
title = [
    "▒█▀▀█ █▀▀ █▀▀ ▀▀█▀▀ ░▀░ █▀▀█ █▀▀█ ░▀░ █▀▀█ ", 
    "▒█▀▀▄ █▀▀ ▀▀█ ░░█░░ ▀█▀ █▄▄█ █▄▄▀ ▀█▀ █░░█",
    "▒█▄▄█ ▀▀▀ ▀▀▀ ░░▀░░ ▀▀▀ ▀░░▀ ▀░▀▀ ▀▀▀ ▀▀▀▀"]

menu :: [[Char]]
menu = [ "INICAR AVENTURA", 
         "   Créditos", 
         "     Sair"]

commandsTable :: [[Char]]
commandsTable = [
    " s / w - mover cursor  ", 
    " f - selecionar        "]

-- cria o menu e imprime ele na tela
printMenu :: [[Char]] -> IO()
printMenu menuTab = do
    let initialBuffer = Scr.createScreenBuffer Scr.width Scr.height Scr.emptyPxl
    let titleBuf = Scr.createBufferFromStringMatrix title
    let menuBuf = Scr.createBufferFromStringMatrix menuTab
    let tableBuf = Scr.createBufferFromStringMatrix commandsTable
    
    
    let tmp2 = Scr.renderCentralized initialBuffer menuBuf 0 10
    let tmp3 = Scr.renderInBuffer tmp2 tableBuf 1 3
    let tmp4 = Scr.renderCentralized tmp3 titleBuf 0 0

    Scr.printScreen tmp4

-- cria o cursor que fica do lado das opcoes do menu
printArrow :: [[Char]] -> Int -> [[Char]]
printArrow [] _ = []
printArrow (row:rest) 0     = (" " ++ row) : rest
printArrow (row:rest) i     = row : printArrow rest (pred i)

mainLoop :: Int -> IO()
mainLoop index = do
    let maxIndex = 3

    printMenu $ printArrow menu index
    
    -- pega um unico caracter da entrada
    hSetBuffering stdin NoBuffering
    command <- getChar
    hSetBuffering stdin LineBuffering

    -- recalcula posicao do cursor
    let newIndex = case command of 'w' -> ((pred index) + maxIndex) `mod` maxIndex
                                   's' -> ((succ index) + maxIndex) `mod` maxIndex
                                   cmd -> index


    if command == 'f' && index == 2 then exitSuccess   -- Encerra o programa.
    else if command =='f' && index == 1 then Crdt.main 
    else if command == 'f' && index == 0 then Mcrt.main -- Entra no menu de interações com as criaturas.
    else putStrLn ""       -- continua no menu

    mainLoop newIndex


main :: IO()
main = mainLoop 0

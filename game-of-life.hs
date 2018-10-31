import Data.List
import Control.Monad
import Control.Concurrent
import qualified System.Process as SP


type Celula = (Int, Int)
type Matriz = [Celula]

-- Logica do jogo
-- Realiza a logica de verificação dos vizinhos
vizinhos :: Celula -> Matriz
vizinhos (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard (dx /= 0 || dy /= 0)
  return (x + dx, y + dy)

-- Realiza a logica dos passos de uma celula 
passo :: Matriz -> Matriz
passo celulas = do
  (novaCelula, n) <- frequencias $ concatMap vizinhos celulas
  guard $ (n == 3) || (n == 2 && novaCelula `elem` celulas)
  return novaCelula

-- Faz uma contagem da frequencia dos vizinhos
frequencias :: Ord a => [a] -> [(a, Int)]
frequencias xs = do
  x <- group $ sort xs
  return (head x, length x)


-- UI
-- Limpa a tela do terminal Linux
clear = putStr "\ESC[2J"

-- Faz o farmato da matriz para ser apresentada via terminal
formatoMatriz :: Matriz -> String
formatoMatriz matriz = do
  y <- ys
  x <- xs
  [marcador x y] ++ eol x
  where
    marcador x y
      | (x, y) `elem` matriz = '*'
      | otherwise          = ' '
    eol x
      | x == maximum xs = ['\n']
      | otherwise       = []

    xs = tamanhoMatriz fst
    ys = tamanhoMatriz snd
    tamanhoMatriz f = [min matriz .. max matriz]
      where
        min = minimum . map f
        max = maximum . map f

-- Função principal para a execução do programa
main = do
  putStrLn "escolha uma forma das listadas:"
  putStrLn "1 - Tetris"
  putStrLn "2 - Blinker"
  putStrLn "3 - Glider"
  putStrLn "4 - Bote"
  putStrLn "5 - Sapo"
  putStrLn "6 - LWSS"
  putStrLn "7 - Plus sign"
  putStrLn "8 - Diehard"
  putStrLn "9 - Acorn"
  opcao <- getChar
    
  case opcao of
    -- Tetris
    '1' -> mapM_ printMatriz . take 20 $ iterate passo [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]
    -- Blinker
    '2' -> mapM_ printMatriz . take 20 $ iterate passo [(1,0), (1,1), (1,2)]
    -- Glider
    '3' -> mapM_ printMatriz . take 20 $ iterate passo [(0,0), (0,1), (0,2), (1,0), (2,1)]
    -- Bote
    '4' -> mapM_ printMatriz . take 20 $ iterate passo [(0,0), (0,1), (1,0), (1,2), (2,1)]
    -- Sapo
    '5' -> mapM_ printMatriz . take 20 $ iterate passo [(0,1), (0,2), (0,3), (1,0), (1,1), (1,2)]
    -- LWSS
    '6' -> mapM_ printMatriz . take 20 $ iterate passo [(0,1), (1,0), (2,0), (3,0), (3,1), (3,2), (3,3), (2,4), (0,4)]
    -- Plus sign
    '7' -> mapM_ printMatriz . take 20 $ iterate passo [(0,1), (1,0), (1,1), (1,2), (2,1)]
    -- Diehard
    '8' -> mapM_ printMatriz . take 20 $ iterate passo [(0,6),(1,0), (1,1), (2,1), (2,5), (2,6), (2,7)]
    -- Acorn
    '9' -> mapM_ printMatriz . take 20 $ iterate passo [(0,1), (1,3), (2,0), (2,1), (2,4), (2,5), (2,6)]
    otherwise -> error "voce escolheu uma opcao inexistente"

-- Cria um delay para imprimir a matriz de forma em que
-- o usuário possa ver com mais calma
printMatriz :: Matriz -> IO ()
printMatriz matriz = do
  putStrLn $ formatoMatriz matriz
  threadDelay 300000
  clear
  putStrLn ""
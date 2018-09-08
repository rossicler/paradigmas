import Data.List
import Control.Monad

type Celula = (Int, Int)
type Matriz = [Celula]

-- Logica do jogo

vizinhos :: Celula -> Matriz
vizinhos (x, y) = do
  dx <- [-1..1]
  dy <- [-1..1]
  guard (dx /= 0 || dy /= 0)
  return (x + dx, y + dy)

passo :: Matriz -> Matriz
passo celulas = do
  (novaCelula, n) <- frequencias $ concatMap vizinhos celulas
  guard $ (n == 3) || (n == 2 && novaCelula `elem` celulas)
  return novaCelula

frequencias :: Ord a => [a] -> [(a, Int)]
frequencias xs = do
  x <- group $ sort xs
  return (head x, length x)


-- UI

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

main = do
  mapM_ printMatriz . take 5 $ iterate passo prototipo
  where
    -- Prototipos listados
    -- Idk
    --prototipo = [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]
    -- Blinker
    --prototipo = [(1,0), (1,1), (1,2)]
    -- Glider
    prototipo = [(0,0), (0,1), (0,2), (1,0), (2,1)]
    -- Bote
    --prototipo = [(0,0), (0,1), (1,0), (1,2), (2,1)]
    -- Sapo
    --prototipo = [(0,1), (0,2), (0,3), (1,0), (1,1), (1,2)]
    -- LWSS
    --prototipo = [(0,1), (1,0), (2,0), (3,0), (3,1), (3,2), (3,3), (2,4), (0,4)]
    -- Plus sign
    --prototipo = [(0,1), (1,0), (1,1), (1,2), (2,1)]
    -- Diehard
    --prototipo = [(0,6),(1,0), (1,1), (2,1), (2,5), (2,6), (2,7)]
    -- Acorn
    --prototipo = [(0,1), (1,3), (2,0), (2,1), (2,4), (2,5), (2,6)]


    printMatriz :: Matriz -> IO ()
    printMatriz matriz = do
      putStrLn $ formatoMatriz matriz
      putStrLn ""
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
  mapM_ printMatriz . take 5 $ iterate passo beacon
  where
    -- Idk
    --beacon = [(0, 0), (1, 0), (0, 1), (3, 3), (2, 3), (3, 2)]
    -- Blinker
    --beacon = [(1,0), (1,1), (1,2)]
    -- Glider
    beacon = [(0,0), (0,1), (0,2), (1,0), (2,1)]

    printMatriz :: Matriz -> IO ()
    printMatriz matriz = do
      putStrLn $ formatoMatriz matriz
      putStrLn ""
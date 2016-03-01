--EJEMPLO--

import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Tree 
import Data.Graph.Inductive.Query.SP 
import Data.Maybe 
import Data.List (transpose) 
 
 
-- Realmente el problema se reduce a leer el formato indicado y convertirlo en datos "usables", 
-- ésto es lo que hace la función "mkGraphFromString", dado un laberinto en la forma indicada, 
-- devuelve los parámetros para poder ser consumidos fácilmente. 
 
-- NOTA: cada comentario corresponde a la línea que se encuentra (explica la línea en que se encuenta) 
 
colossusTimes :: String ->      -- Dada una cadena con el laberinto en el formato indicado, devuelve una tupla con: 
                     (Int       --    El tiempo que tardará el Maligno Pachus en matar a la Princesa. 
                     ,Int)      --    El tiempo que tardará Colossus en llegar a la Princesa (o 0 si es imposible) 
colossusTimes ws = (t 
                   , length $                                           -- El tiempo de Colossus equivale a la longitud 
                      sp (cell 'C') (cell 'O')                          -- de la ruta de 'C' a 'O' en el grafo 
                       (mkGraph fnodes                                  -- de los nodos y las aristas 
                         (concatMap (\e@(a, b, c) -> [e, (b, a, c)])    -- en ambos sentidos 
                            (hedges _nodes ++                           -- de las horizontales 
                            (hedges.transpose) _nodes)) :: Gr Char Int) -- y las verticales 
                   ) 
  where (x:xs) = lines ws                                            -- Separamos la 1ª línea. 
        (w:h:t:_) = map (read :: String -> Int) $ words x            -- Leemos datos de la 1ª línea. 
        _nodes = [[(a*w+b,c)|(b,c)<-zip[0..]as]|(a,as)<-zip[0..]xs]  -- Enumera las celdas: 0, 1, 2, ... 
        fnodes = concat _nodes                                       -- Las celdas unidimensional. 
        hedges ns = concatMap (\x -> constructEdges x $ tail x) ns   -- Calcula aristas horizontalmente. 
        constructEdge (a, u) (b, v)                                  -- Dadas dos celdas ¿se puede pasar? 
            | u == 'X' || v == 'X' = Nothing 
            | otherwise            = Just (a, b, 1) 
        constructEdges x y = catMaybes $ zipWith constructEdge x y   -- Mezcla celdas para buscar aristas. 
        cell x = fst $ head $ filter ((==x).snd) fnodes              -- Busca una celda. 
 
 
 
 
 
-- Resolver el problema entonces es trivial 
colossusRescueTime :: String -> Int 
colossusRescueTime = computeTime . colossusTimes 
  where computeTime (t, c) | c == 0 || c > t + 1 = -1           -- Ouch! 
                           | otherwise           = c - 1        -- Uffff... 
 
 
 
 
 
-- Por ejemplo: 
 
sampleData = unlines ["5 5 8" 
                     ,"XXXXX" 
                     ,"XCLLX" 
                     ,"XXXLX" 
                     ,"XOLLX" 
                     ,"XXXXX" 
                     ] 
 
{-- 
*Main> colossusRescueTime sampleData  
6 
--} 
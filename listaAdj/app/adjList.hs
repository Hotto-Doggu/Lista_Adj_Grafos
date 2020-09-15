-------------------------------------------------------------------------------------------------------------------------------------------------------------
--Lógica do programa:

  --construindo lista de adjacencias de acordo com a condição de direcionamento:
  --passa pela lista de vertices, construindo uma lista de tuplas no formato [(origem,[destinos],[pesos])..]
    --se não direcionado: listar todos os vertices, e suas adjacencias (vertices com quais compartilha aresta),
      --em lista no formato [(vertice,[adjacentes],[pesos])..]                                                      ex: 1->4 == 4->1, ambos são listados como mesma relação
    --se direcionado: listar apenas os vertices 'origem' e suas adjacencias (vertices 'destino' de suas arestas).   ex: 1->4 \= 4->1, listados como relações diferentes

--Para rodar o programa você pode compilar e então rodar o executável produzido fazendo    ghc --make adjList    e então    ./adjList
--ou pode usar o comando runhaskell assim:   runhaskell adjList.hs   e rodar o programa dinâmicamente
-------------------------------------------------------------------------------------------------------------------------------------------------------------
module Main where

import           Data.List
import           Data.Char
import           Control.Monad

type Vertice   = Int
type Aresta    = (Vertice,Vertice,Int)      --(origem,destino,peso)
type Adjacency = (Vertice,[Vertice],[Int])  --(origem,[destinos],[pesos])

---------------------------------------------------------------Parte 'Impura' do programa--------------------------------------------------------------------
main = do
  inp <- getLine                             --recebe string de entrada "n m b"
  let input = map read $ words inp :: [Int]  --formata entrada para [n,m,b]
  inputs <- replicateM (last input) getLine                                 --recebe quantidade 'm' de strings como entrada (lista de strings)
  let formatedInputs = map (\line -> map read $ words line :: [Int]) inputs --formata entrada para [[o,d,p]]
  let tupledInputs = map toTriple formatedInputs                            --formata [[o,d,p]] em [(o,d,p)]
  --1º lendo parametros [n,m,b] do grafo --> n vertices, m arestas, direcionado\não-direcionado (1 ou 0)
  --2º lendo m vertices [origem,destino,peso], resultando em uma lista de vertices

--utiliza da avaliação lazy do haskell em nosso favor, computando os dados conforme são requisitados para expressão em terminal apenas.
  if (last input) == 1 then do mapM_ putStrLn ((header input):formatImpress (direcionado   tupledInputs))  --imprime cabeçalho + a execução da lista de adjacencias direcionada
                       else do mapM_ putStrLn ((header input):formatImpress (n_direcionado tupledInputs))  --imprime cabeçalho + a execução da lista de adjacencias não direcionada

---------------------------------------------------------------Parte 'Pura' do programa----------------------------------------------------------------------
--Auxiliares:
fst3 :: (a,b,c) -> a  --funçoes polimórficas para trabalhar sobre as estruturas de vertice Aresta ou Adjacency
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b  -- ||
snd3 (_,y,_) = y

trd3 :: (a,b,c) -> c  -- ||
trd3 (_,_,z) = z

(.<.) :: Aresta -> Aresta -> Bool  --funções específicas para trabalhar com comparação de Aresta
(o1, d1, p1) .<. (o2, d2, p2) = o1 < o2 || (o1 == o2 && d1 < d2)

(.>.) :: Aresta -> Aresta -> Bool  -- ||
(o1, d1, p1) .>. (o2, d2, p2) = o1 > o2 || (o1 == o2 && d1 > d2)

toTriple :: [Int] -> (Int, Int, Int) --função que transforma lista de três valores Int em tupla de três valores equivalente
toTriple [o,d,p] = (o,d,p)

(.<<.) :: Adjacency -> Adjacency -> Bool  --funções específicas para trabalhar com comparação de Adjacency
(o1, (d1:d1s), p1) .<<. (o2, (d2:d2s), p2) = o1 < o2 || (o1 == o2 && d1 < d2)

(.>>.) :: Adjacency -> Adjacency -> Bool  -- ||
(o1, (d1:d1s), p1) .>>. (o2, (d2:d2s), p2) = o1 > o2 || (o1 == o2 && d1 > d2)

-------------------------------------------------------------------------------------------------------------------------------------------------------------
--1º Passo: Pré-Procesamento
--Agrupar as arestas de mesma origem dentro da lista, para facilitar processamento de adjacencias.
sortEntries :: [Aresta] -> [Aresta] --quicksort adaptado com devidas funções de comparação
sortEntries []     = []
sortEntries (p:xs) = (sortEntries lesser) ++ [p] ++ (sortEntries greater)
     where lesser  = filter (.<. p) xs
           greater = filter (.>. p) xs
--que ordena uma lista de arestas utilizando as funções de comparação de arestas, definidas na secção de funções auxiliares.


--2º Passo: Conversão em Lista de Adjacencias
--checa o vertice origem contra as adjacencias ja encontradas: se está no acumulador, atualiza a respectiva adjacencia ; se não, insere nova instancia de adjacencia e
--segue com analise da lista de arestas.
arestas2adj :: [Aresta] -> [Adjacency] -- [(origem,destino,peso)] -> [(origem,[destinos],[pesos])]
arestas2adj = foldr step []
            where step aresta acc = if isPart aresta acc                                  --se origem ja está no acumulador como relação de adjacencia,
                                    then update aresta acc                                --atualiza a relação de adjacencia com destino e peso da aresta;
                                    else (fst3 aresta, [snd3 aresta], [trd3 aresta]):acc  --se não está, inclui aresta como nova relação de adjacencia (devida formatação).
                  isPart a b = (fst3 a) `elem` (map fst3 b)  --verifica se origem da aresta já se apresenta em alguma das relações
                  update x y = map (\(orig,dests,pesos) -> if fst3 x == orig then (orig, (snd3 x):dests, (trd3 x):pesos) else (orig,dests,pesos))  y
                  --mapeia a comparação de origens sobre a lista até achar o elemento desejado, e então o atualiza; re-insere os outros elementos sem alteração


--3º Passo (para grafos não direcionados):
--Inverte a relação origem->destino em cada aresta da lista, e então aplica novamente a função arestas2adj para obter o complemento da lista de adjacencias direcionada.
--Em seguida, concatena as duas listas para obter a listagem completa de adjacencias não-direcionadas, e as agrupa por ordenação novamente (para simplificar a impressão).
inverterDir :: [Aresta] -> [Aresta]
inverterDir = map (\(origem,destino,peso) -> (destino,origem,peso))

formataDir :: [Aresta] -> [Adjacency]
formataDir lista = sortAdj ( (arestas2adj lista) ++ (arestas2adj $ inverterDir lista) )
                 where sortAdj []     = []
                       sortAdj (p:xs) = (sortAdj less) ++ [p] ++ (sortAdj great)
                                          where less  = filter (.<<. p) xs
                                                great = filter (.>>. p) xs

--Formato Final:
direcionado :: [Aresta] -> [Adjacency] --ordena arestas e então converte em relações de adjacencia.
direcionado = arestas2adj . sortEntries

n_direcionado :: [Aresta] -> [Adjacency] --ordena arestas e então as converte em relações de adjacencia, formatando para obter a listagem completa de cada vértice.
n_direcionado = formataDir . sortEntries
-------------------------------------------------------------------------------------------------------------------------------------------------------------
--imprimindo: n m DIRECIONADO\NAO DIRECIONADO --> primeira linha (header)
--            o d p                           --> origem destino peso
--            ...                             --> repetir ate imprimir todas as arestas
--para imprimir, primeiro putStrLn (header input), e então putStrLn [strings de arestas]

header :: [Int] -> String --[n,m,b] -> "n m dir\n_dir"
header input = (show (head input) ++ " " ++ show (input !! 1) ++ dir)
  where dir | (last input) == 1 = " DIRECIONADO"      --b==1
            | otherwise         = " NAO DIRECIONADO"  --b==0

formatImpress :: [Adjacency] -> [String] --[(origem,[destinos],[pesos])] -> ["origem destino peso"]
formatImpress adj = concat $ [[show o ++ " " ++ show d ++ " " ++ show p | o<-(map fst3 adj)] | d<-(concat . map snd3 $ adj), p<-(concat . map trd3 $ adj)]
--transforma cada relação de adjacencia em suas respectivas arestas, pegando a origem, um item da lista de destinos e seu peso correspondente, e tornando esta combinação
--uma string "origem destino peso" na lista. Cada origem resulta em uma lista de arestas com mesma origem, então temos de 'achatar' esta lista de listas, obtendo assim uma
--lista de strings. Na parte de I\O do código aplicaremos a função putStrLn sobre essa lista, o que imprimirá no terminal cada string de aresta como uma linha ed saída.

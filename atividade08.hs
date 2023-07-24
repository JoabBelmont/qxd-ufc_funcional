-- MÓDULOS

import System.IO
import System.Environment
-- não import mais nada!

--IDENTIFICAÇÃO

atividade = 8
matricula = "539029"
nome      = "Antônio Joabe Alves Morais"

-- ATIVIDADE

-- Construir programa que leia
-- na linha de comando três strings.
-- A primeira é um nome de arquivo válido, 
-- digamos, f. As outras são palavras,
-- w1 e w2. O programa deve susturuir
-- todas as aparições de w1 em f
-- por w2. Um arquivo de saída com as
-- modificações deve ser
-- a saída. Seu nome precisa se o de f
-- o arquivo de saída deter nome igual
-- com prefixo "subst-". .

-- MATENHA O .hs COM NOME
-- "atividade.hs" E CONSEQUENTEMENTE
-- EXECUTÁVEL COMO SENDO
-- "atividade08".

-- CÓDIGO


main = do
     args <- getArgs
     let inputFile = head args
     let targetWord = args !! 1
     let newWord = args !! 2
     let outputFile = "subst-" ++ inputFile

     contents <- readFile inputFile
     let modifiedContents = replaceWord targetWord newWord contents
     writeFile outputFile modifiedContents

     return ()

replaceWord :: String -> String -> String -> String
replaceWord _ _ [] = []
replaceWord w1 w2 str@(c:cs)
     | take (length w1) str == w1 = w2 ++ replaceWord w1 w2 (drop (length w1) str)
     | otherwise = c : replaceWord w1 w2 cs

-- INFORMAÇÕES

-- Compilação e execução

-- $ ghci atividade-08.hs
-- $ ./atividade-08 historia.txt Pedro Pablo

-- Onde "historia.txt" é um arquivo de texto
-- em que toda palavra "Pedro" é substituída
-- por "Pablo".

-- Exemplo

-- "historia.txt" de entrada,

-- Pedro vivia numa casa de pedra.
-- Mas Pdro queria morar numa 
-- casa de ouro. Pobre Pedro!

-- "subst-historia.txt" criado,

-- Peblo vivia numa casa de pedra.
-- Mas Pabloqueria morar numa 
-- casa de ouro. Pobre Pablo!

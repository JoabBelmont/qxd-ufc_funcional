-- IDENTIFICAÇÃO
matricula = "539029" -- coloque a matricula aqui entre as aspas

-- Nome
nome = "Antônio Joabe Alves Morais" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 2

-- Esta atividade visa construir uma 
-- função que determine os n primeiros números primos

-- Construa as funções a seguir,

-- determina os divisores de x excluindo o 1
divisores :: Int -> [Int]
divisores x = [y | y <- [2..x], x `mod` y == 0]

-- Determina se um número x é ou não primo
eprimo :: Int -> Bool
eprimo x = divisores x == [x]

eprimo' :: Int -> Bool
eprimo' x = length (divisores x) == 1

-- cria lista com n primeiros primos
primos :: Int -> [Int]
primos n = take n (filter eprimo [2..])

primos' :: Int -> [Int]
primos' n = take n [x | x <- [2..], eprimo x]

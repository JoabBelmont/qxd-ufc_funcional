-- IDENTIFICAÇÃO

atividade = 7

nome = "Antônio Joabe Alves Morais"

matricula = "539029"

-- TIPO DE DADOS

-- Representa polinômio como 
-- um vetor de seus coeficientes

-- através de seus coeficientes

data Poly = Poly [Float]

-- IMPLEMENTAR

-- Instância de Show que permite 
-- imprimir um polinômio

instance Show Poly where
  show (Poly coeffs) = formatPoly coeffs 0
    where
      formatPoly :: [Float] -> Int -> String
      formatPoly [] _ = ""
      formatPoly (x:xs) i
        | x == 0 = formatPoly xs (i + 1)
        | i == 0 = show x ++ formatPoly xs (i + 1)
        | i == 1 = showCoeff x ++ "x" ++ formatPoly xs (i + 1)
        | otherwise = showCoeff x ++ "X^" ++ show i ++ formatPoly xs (i + 1)
      
      showCoeff :: Float -> String
      showCoeff coeff
        | coeff > 0 = "+" ++ show coeff
        | otherwise = show coeff

-- Exemplos
-- Main> Poly [1,2,3]
-- 1.0+2.0x+3.0X^2
-- *Main> Poly [-2,1,0]
-- -2.0+1.0x
-- *Main> Poly [-1,0,-1]
-- -1.0-1.0X^2


--AVALIAÇÃO DE POLINÔMIOS

-- Avalia um poliômio P 
-- dado x, ou seja, calcula P(x) 

avalPoly :: Poly -> Float -> Float
avalPoly (Poly coeffs) x = sum (zipWith (\c n -> c * (x^n)) coeffs [0..])

-- Exemplos
-- *Main> avalPoly (Poly [1,2,3]) 5
-- 86.0
-- *Main> avalPoly (Poly [-1,1,3]) 5
-- 79.0
-- *Main> avalPoly (Poly [11,0,2,2]) 3
-- 83.0

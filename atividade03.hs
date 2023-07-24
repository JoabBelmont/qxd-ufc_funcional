-- IDENTIFICAÇÃO
matricula = "539029" -- coloque a matricula aqui entre as asspas

-- Nome
nome = "Antônio Joabe Alves Morais" -- coloque seu nome aqui entre aspas

-- ATIVIDADE 3

-- Remove espaços existentes no início
-- e final de uma strings dada.

strip :: [Char] -> [Char]
strip xs
    | null xs = []
    | head xs == ' ' = strip (tail xs)
    | last xs == ' ' = strip (init xs)
    | otherwise = xs

-- Separa a primeira palavra do restante
-- de uma string (Palavras são substeings 
-- separadas por espaços). Exemplo,

-- >> popWord "casa  de tijolos"
-- ["casa", "de tijolos"]'
-- >>

popWord :: [Char] -> ([Char], [Char])
popWord xs =
    let (word, rest) = span (/= ' ') xs
    in (word, strip rest)

-- Processa uma string e retorna 
-- a lista de suas palavras. OBS: 
-- palavras não devem ter espaços 
-- extemos e nem serem vazias. Exemplo,

-- >> splitStr " The   fox jumped  "
-- ["The", "fox", "jumped"]

splitStr :: [Char] -> [ [Char] ]
splitStr xs
    | null xs = []
    | otherwise = word : splitStr rest
      where (word, rest) = popWord (strip xs)

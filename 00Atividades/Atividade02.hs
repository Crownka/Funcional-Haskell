atividade = "2"

-- Substitua seus dados
nome = "Patrick de Farias Ramos"
matricula = "556711"

-- 1 
-- Sejam as tuplas u e v de inteiros
-- tal que exista um inteiro k onde
-- u = kv ou v = ku  
-- então u e v são mútiplos. Construa 
-- função que determine se duas 
-- tuplas de inteiros  são múltiplas.
isMult :: (Int,Int) -> (Int, Int) -> Bool

isMult (a,b) (c,d) = (c `mod` a == 0 && d `mod` b == 0) || (d `mod` a == 0 && c `mod` b == 0)

   
-- 2
-- Sejam todos os triângulos retângulos
-- de perímetro p e de lados inteiros.
--   representados por tuplas (a,b,c) 
-- com  a>=b>=c. Criar  
--  função que determine 
-- o total destes triângulos dado p .

tot'tri  :: Int -> Int

tot'tri p = length [(a, b, c) | a <- [1 .. p], b <- [1 .. a], c <- [1 .. b], a + b + c == p, a + b + c == p, a^2 == b^2 + c^2]

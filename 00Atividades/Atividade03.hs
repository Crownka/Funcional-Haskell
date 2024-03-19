atividade = "3"
nome = "Patrick de Farias Ramos"
matricula = "*556711"



tls :: String -> [(Char, Int)]
tls s = nubBy (\(x,)(y,) -> x == y) result
    where
        result = zip s (comparator s)
        count a s = length [x | x <- s, x == a]
        comparator s = [count c s | c <- s]

------------------------------------------------------

filterIsAlpha :: String -> String
filterIsAlpha = filter (\ch -> isAlphaNum ch && notElem ch ['0' .. '9'] || ch == ' ')

tlsString :: [String] -> [(String, Int)]
tlsString [] = []
tlsString (x:xs) = (x, length (filter (==x) (x:xs))) : tlsString (filter (/=x) xs)

maximumBy_ ::[(String, Int)] -> (String, Int)
maximumBy_ [] = error "maximum of empty list"
maximumBy_ (x:xs) = foldl (\(maxStr, maxInt) (str, int) -> if int > maxInt then (str, int) else (maxStr, maxInt)) x xs

sfq :: String -> (String, Int)
sfq str = maximumBy_ (tlsString (words (filterIsAlpha str)))

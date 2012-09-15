--9781800
	
import Data.Char
type PublicKey = (Integer, Integer)

type PrivateKey = (Integer, Integer)

sampleMessage = [
    7321434,3598567,1716197,4119550,1716197,1348875,
    8435094,8273804,543720,4505902, 4941202,6726051,
    8083299,4896702,3790340,8176577,3956251,3855025,
    9779898,9626936,7581573,530038, 5596561,1789054,
    3956251,3855025,9779898,1631514,8435094,9779898,
    60307,  7379435 ]

main = do  
    putStrLn( "Welcome to RSA on Haskell 0.1\n"++
             "Moechten sie: \n"++
		     "\t\ta) Eine Nachricht verschluesseln\n"++
			 "\t\tb) Eine Nachricht entschluesseln\n"++
			 "\t\tq) Programm beenden")
    (choice:xs) <- getLine
    putStrLn ("Sie haben sich fuer "++[choice]++" entschieden.");
	work choice
	
work c 
 | c == 'a' = encode
 | c == 'b' = decode
 | c == 'q' = quit
 | otherwise = wrongInput

wrongInput = do
		putStr("Falscher Input")
		msg <- getLine
		putStrLn msg

encode = do
		putStr("Bitte oeffentlichen Schluessel eingeben ( Format: d n ):")
		key <- getLine
		putStr("Bitte Nachricht eingeben: ")
		msg <- getLine
		putStrLn (concat (map (\x -> show (fromInteger x)++[' ']) (encodeRSA (parseKey key) msg)))

decode = do
		putStr("Bitte privaten Schluessel eingeben ( Format: e n ):")
		key <- getLine
		putStr("Bitte verschluesselte Nachricht eingeben: ")
		msg <- getLine
		putStrLn (decodeRSA (parseKey key) (parseEncrypted msg))
		
quit   = do
        putStrLn "Auf Wiedersehen"

parseKey :: String -> (Integer,Integer)--(Integer,Integer)
parseKey x = ( toInteger (convert(takeWhile (/= ' ') x)),toInteger (convert(tail (dropWhile (/= ' ') x))))--(0,0)

parseEncrypted :: String -> [Integer]
parseEncrypted msg = map (\x -> toInteger (convert x)) msgParts
  where msgParts = breakUp msg ' '

breakUp :: String -> Char -> [String]
breakUp str c = breakUpH str c "" []

breakUpH :: String -> Char -> String ->[String] -> [String]
breakUpH [] c act acc = acc ++ [act]
breakUpH (x:xs) c act acc
 | x == c = breakUpH xs c "" (acc ++ [act])
 | otherwise = breakUpH xs c (act++[x]) acc



encodeRSA :: PublicKey -> String -> [Integer]
encodeRSA (a,b) x = map (\y -> toInteger(modInteger(y^a) b)) convertedList
  where convertedList = convertToBase96 (groupBy2 x)

decodeRSA :: PrivateKey -> [Integer] -> String
decodeRSA (a,b) x = concat parts
  where parts = map (\y -> fromIntegerToString(modInteger(y^a) b)) x

fromIntegerToString :: Integer -> String
fromIntegerToString x 
  | a == 96 && b == 96 = [(chr (32)),(chr (32))]
  | a == 96 = [(chr (32)),(chr (b+32))]
  | b == 96 = [(chr (a+32)),(chr (32))]
  | otherwise = [(chr (a+32)),(chr (b+32))]
  where (a,b) = convertFromBase96 x

convertFromBase96 :: Integer -> (Int ,Int)
convertFromBase96 x = ((div (fromInteger x) 96), (mod (fromInteger x) 96))

modInteger :: Integer -> Integer -> Integer
modInteger a b = toInteger (mod (fromInteger a) (fromInteger b))

ordInteger :: Char -> Integer
ordInteger x = toInteger(ord x)

groupBy2 :: String -> [String]
groupBy2 [] = []
groupBy2 (x:[]) = [[x,' ']]
groupBy2 (x:y:z) = [[x,y]] ++ (groupBy2 z)

convertToBase96 :: [String] -> [Integer]
convertToBase96 [] = []
convertToBase96 (x:xs) = [toInteger ( ((ord (head x)) -32)*96 + ((ord (last x))-32))] ++ (convertToBase96 xs)

convert :: String  -> Int
convert [] = 0
convert x = (ord ( head x)-48)*10^((length x)-1) + convert (tail x)

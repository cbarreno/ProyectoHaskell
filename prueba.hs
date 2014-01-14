-- main = do   print True
            -- print 2
            -- print "haha"
            -- print 3.2
            -- print [3,4,3]
			
			
capital :: String -> String
capital "" = "¡Una cadena vacía!"
capital all@(x:_) = "La primera letra de " ++ all ++ " es " ++ [x]		

main2 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main2
        else return ()	
		
		
main02 = do
    putStrLn "Hello, what's your name?"
    name <- getLine
    putStrLn ("Hey " ++ name ++ ", you rock!")

putCharLn :: Char -> IO ()
putCharLn c = do putChar c
                 putChar '\n'
				 
main = do
  putChar 't'
  putChar 'e'
  putCharLn 'h'
  
  
main4 = do
    c <- getChar
    if c /= ' '
        then do
            putChar c
            main4
        else return () 
		
		
main5 = do
    a <- getLine
    b <- getLine
    c <- getLine
    print [a,b,c]
	
main6 = do
    rs <- sequence [getLine, getLine, getLine]
    print rs
	
-- main7 = forever $ do
    -- putStr "Give me some input: "
    -- l <- getLine
    -- putStrLn $ map toUpper l	
	
	
reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]	

describeList :: [a] -> String
describeList xs = "La lista es" ++ case xs of []  -> "una lista vacía."
                                              [x] -> "una lista unitaria."
                                              xs  -> "una lista larga."
											  
zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys				


elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
    | a == x    = True
    | otherwise = a `elem'` xs							  
	
pon :: Int -> IO()	
pon b= 
		do
			print (b*5)
			
main07 = do
    return ()
    return "HAHAHA"
    line <- getLine
    return "BLAH BLAH BLAH"
    return 4
    putStrLn line

main08 = do
    a <- return "hell"
    b <- return "yeah!"
    putStrLn ( a ++ " " ++ b	)
			
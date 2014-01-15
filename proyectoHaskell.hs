{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
-- import Text.XML.HXT.Core
import System.IO
import Data.List

-- Fuente: http://www.haskell.org/haskellwiki/HXT/Practical/Simple2 
-- This example demonstrates a more complex XML parse,
-- involving multiple levels, attributes, inner lists,
-- and dealing with optional data.
 
-- Example data drawn from:
-- http://www.ibiblio.org/xml/books/bible/examples/05/5-1.xml
-- save as: simple2.xml
 
data Device = Device 
  { idD, userAgent, fallBack :: String,
    groups :: [Group] }
  deriving (Show, Eq)
 

data Group = Group
  { idG :: String,
	capabilitys :: [Capability]}
	deriving (Show, Eq)

data Capability = Capability
    {name,value :: String }
  deriving (Show, Eq)
 
-- parseXML file = readDocument [ withValidate no
                             -- , withRemoveWS yes  -- throw away formating WS
                             -- ] file
 
-- atTag tag = deep (isElem >>> hasName tag)
  
  
 
-- Incremental development of the getTeams function:
 
-- Try capturing some statistics about the players
 
-- significant = not . all (`elem` " \n\r\t")
 
-- Use our definition of "significant" strings to
-- capture the value; or else nothing.
 
-- getStat attr = 
  -- (getAttrValue attr >>> isA significant >>> arr Just)
    -- `orElse` (constA Nothing)

-- getCapabilities = atTag "capability" >>>
  -- proc c -> do
    -- name    <- getAttrValue "name" -< c
    -- value    <- getAttrValue "value"    -< c
    -- returnA -< Capability
      -- { name = name,
        -- value = value}

-- getGroups = atTag "group" >>>
  -- proc p -> do
    -- id    <- getAttrValue "id" -< p
    -- capabilitys  <- listA getCapabilities  -< p
    -- returnA -< Group
      -- { idG = id,
        -- capabilitys  = capabilitys}
	 
-- getDevices = atTag "device" >>>
  -- proc l -> do
    -- id <- getAttrValue "id" -< l
    -- userAgent <- getAttrValue "user_agent" -< l
    -- fallBack <- getAttrValue "fall_back" -< l
    -- groups  <- listA getGroups    -< l
    -- returnA -< Device
      -- { idD   = id,
        -- userAgent = userAgent,
        -- fallBack = fallBack,
        -- groups     = groups}
 
-- Our final choices


--MyShow s =  concat  [ "[" , intersperse ',' s ,  "]" ]
data Arbol a = Hoja | Nodo [a] [Arbol a ]	
			   deriving (Show, Eq)
			   
-- data Etiqueta a = Nombre 
			   -- deriving (Show, Eq)
			   
						
-- data Device = Device 
  -- { idD, userAgent, fallBack :: String,
    -- groups :: [Group] }
  -- deriving (Show, Eq)
 			   
			   			   
-- listaAr::String -> {Arbol a}
-- listaAr = [Arbol a]

		
-- a1 = Nodo 10 [a11,a13]
	-- where
	-- a11 = Nodo [22,9] [Hoja,Hoja]
	-- a12 = Nodo [35] [Hoja]
	-- a13 = Nodo [52] [Hoja]

-- 
	 
crearArbol :: [a] -> Arbol a
crearArbol []=Hoja
crearArbol (x:xs) 
	| length (x:xs)==1 = Nodo [x] [Hoja]
	|otherwise = Nodo [x] [crearArbol (xs)]
	-- pertenece :: Eq a => a -> Arbol a -> Bool
-- pertenece _ Hoja = False
-- pertenece x (Nodo r xs) = x == r || or (map (pertenece x ) xs)
--tipoEtiqueta :: [a] -> a
								 
compruebaEtiqueta0 []= 0					
compruebaEtiqueta0 (x:xs) 
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] && last (x:xs) == ">" && last (init (x:xs))=="/" 	=  1
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] &&  last (x:xs) == ">" && last (init (x:xs))=="\"" = 2
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] &&  last (x:xs) == ">" && removeNon (last (init (x:xs))) == ["1"] = 2
	| head (x:xs) == "<" && head (tail (x:xs)) == "/" &&  last (x:xs) == ">" && removeNon (last (init (x:xs))) == ["1"]  = 3
	|otherwise = 4
	--insertarArbol :: Ord a => a -> Arbol a -> Arbol a
insertarArbol e Hoja = Nodo e [Hoja]
insertarArbol e (Nodo x xs)
  | xs == [Hoja] = Nodo x (tail (xs ++ [Nodo e [Hoja]]))
  | xs==xs = Nodo x (xs ++ [Nodo e [Hoja]])
 
insertarNodoenArbol Hoja (Nodo x xs) = (Nodo x xs)
insertarNodoenArbol nodo (Nodo x xs)
  | xs == [Hoja] = Nodo x (tail (xs ++ [nodo]))
  | xs==xs = Nodo x (xs ++ [nodo])
 
quitaEspacio x
	| x==[] = []
	| x==x =lines(unwords(words( unwords [head x]))) ++ quitaEspacio (tail x)
	
parse0 [] = (0,Hoja)
parse0 (x:xs) =	do	
					let var=compruebaEtiquetaVersion (words( myShow (head (x:xs))))
					let lon=length (x:xs)
					case var of 
								1 -> do 
										let a= Nodo [x] [Hoja]
										if lon==1
											then (1,a)
											else do 
												parse1 (xs) a []
								2 -> (2,Hoja)
								0 -> (0,Hoja)
							
parse1 (x:xs) a listanombres= do
									let lon=length (x:xs)
									let var=compruebaEtiqueta0 (words( myShow (head (x:xs))))
									case var of 
												0 -> (0,Hoja)
												2 -> do
														let dupla=obtenerNombre(words( myShow (head(words(head (x:xs)))))) 
														let nombreEtiqueta=snd dupla
														let list=listanombres ++ [nombreEtiqueta]
														--let otr0="p"--nombreEtiqueta
														case nombreEtiqueta of 
																				"devices" ->	do
																									   let arbol= Nodo [nombreEtiqueta] [Hoja]
																									   let tupla =parse1 (xs) arbol list
																									   let n= insertarNodoenArbol (snd tupla) a
																									   (fst(tupla),n)
																				"device" ->	do
																									   let id=obtenerIdD (words( myShow (unwords(tail(words(head (x:xs))))))) 
																									   let user=obtenerUserAgentG (fst id)
																									   let fallback=obtenerFallBackD (fst user)
																									   let arbol= Nodo [nombreEtiqueta,snd id,snd user,snd fallback] [Hoja]
																									   let tupla =parse1 (xs) arbol list
																									   let n= insertarNodoenArbol (snd tupla) a
																									   (fst(tupla),n)					   
																				"group" ->	do
																									   let id=obtenerIdG (words( myShow (unwords(tail(words(head (x:xs))))))) 
																									   let arbol= Nodo [nombreEtiqueta,snd id] [Hoja]
																									   let tupla =parse1 (xs) arbol list
																									   let n= insertarNodoenArbol (snd tupla) a
																									   (fst(tupla),n)
																				"version" ->	do
																									   let sinversion=devuelveSinVersion (xs)
																									   let tupla =parse1 (sinversion) a list
																									   (fst(tupla),snd (tupla))					   
																			    					   				   
																				otr0 ->	do
																									   let tupla =parse1 (xs) a list
																									   (fst(tupla),snd (tupla))
																									
																									   
																				
												1 -> do 
														let dupla=obtenerNombre(words( myShow (head(words(head (x:xs))))))
														let etiquet=snd (dupla)
														case etiquet of 
																		"capability" ->	  do
																							let name=obtenerNombreC  (words( myShow (unwords(tail(words(head (x:xs)))))))
																							let value=obtenerValueC (fst name)
																							let n= insertarArbol [snd dupla ,snd name,snd value] a
																							if lon==1
																								then (1,n)
																								else do 
																									parse1 (xs) n listanombres
																		otr0 -> do
																						if lon==1
																								then (1,a)
																								else do 
																									parse1 (xs) a listanombres
												3 -> do
														let lonl=(length listanombres)==1
														let upla=obtenerNombre(words( myShow (head(words(head (x:xs)))))) 
														let nombreEtiqueta=snd upla
														let re=(last (listanombres )== nombreEtiqueta)
														case re of 
																	True -> case lonl of 
																						True -> if lon ==1
																									then (1,a)
																									else do parse1 (xs) a []
																						False ->  if lon ==1
																										then (2,Hoja)
																										else do parse1 (xs) a (init(listanombres))
																	False -> (2,Hoja)
												4 -> (2,Hoja)
										 
								
					


					
obtenerNombre []=([],"")								
obtenerNombre (x:xs)
	 | x=="<" && head(xs) == "?" = obtenerNombre (tail(xs))
	 | x== "<" && head(xs)=="/" =obtenerNombre (tail(xs) )
	 | x=="<"&& removeNon (head (tail (x:xs))) == ["1"] = obtenerNombre(xs)  
	 | x==">"&& x=="/" =((xs),"")
	 | x==">" =((xs),"")
	 | x==" " =((xs),"")
	 | x=="="= ((xs),"")
	 | otherwise = (fst(obtenerNombre(xs)) ,x ++ snd (obtenerNombre(xs)))		

obtenerDatos []=([],"")								
obtenerDatos(x:xs)
	 | x=="="&&head(xs)=="\""&& removeNon (head(tail(xs)))==["1"] = (fst (obtenerDatos(tail(tail(xs)))) ,head(tail (xs)) ++ snd(obtenerDatos(tail(tail(xs)))))
	 | x=="="&&head(xs)=="\""&& head(tail(xs))=="\"" = (tail(tail(xs)),"")
	 | x=="="&&head(xs)=="\""&& isNum (head(tail(xs)))==["1"] = (fst (obtenerDatos(tail(tail(xs)))) ,head(tail (xs)) ++ snd(obtenerDatos(tail(tail(xs)))))
	 | x=="\"" =((xs),"")
	 | otherwise = (fst(obtenerDatos(xs)) ,x ++ snd (obtenerDatos(xs)))	
obtenerNombreC []=([],"")								
obtenerNombreC (x:xs)
	 | x=="n" && head(xs) == "a"&& head(tail(xs))=="m" && head(tail(tail(xs))) == "e" = obtenerDatos(tail(tail(tail(xs))))--obtenerNombre (tail(xs))
	 | otherwise = ([],"error")		
	 
obtenerValueC []=([],"")								
obtenerValueC (x:xs)
	 | x=="v" && head(xs) == "a"&& head(tail(xs))=="l" && head(tail(tail(xs))) == "u" && head(tail(tail(tail(xs))))=="e" = obtenerDatos(tail(tail(tail(tail(xs)))))--obtenerNombre (tail(xs))
   	 | otherwise = ([],"error")	
	 
obtenerIdG []=([],"")								
obtenerIdG(x:xs)
	 | x=="i" && head(xs) == "d"  = obtenerDatos(tail(xs))
   	 | otherwise = ([],"error")	
	 
obtenerIdD []=([],"")								
obtenerIdD(x:xs)
	 | x=="i" && head(xs) == "d"  = obtenerDatos(tail(xs))
   	 | otherwise = ([],"error")		
obtenerFallBackD []=([],"")								
obtenerFallBackD (x:xs)
	 | x=="f" && head(xs) == "a"&& head(tail(xs))=="l" && head(tail(tail(xs))) == "l" && head(tail(tail(tail(xs))))=="_" && head(tail(tail(tail(tail(xs)))))=="b" && head(tail(tail(tail(tail(tail(xs))))))=="a" && head(tail(tail(tail(tail(tail(tail(xs)))))))=="c" && head(tail(tail(tail(tail(tail(tail(tail(xs))))))))=="k"  = obtenerDatos(tail(tail(tail(tail(tail(tail(tail(tail(xs)))))))))
	 | otherwise = ([],"error")	
obtenerUserAgentG []=([],"")								
obtenerUserAgentG (x:xs)
	| x=="u" && head(xs) == "s"&& head(tail(xs))=="e" && head(tail(tail(xs))) == "r" && head(tail(tail(tail(xs))))=="_" && head(tail(tail(tail(tail(xs)))))=="a" && head(tail(tail(tail(tail(tail(xs))))))=="g" && head(tail(tail(tail(tail(tail(tail(xs)))))))=="e" && head(tail(tail(tail(tail(tail(tail(tail(xs))))))))=="n" && head(tail(tail(tail(tail(tail(tail(tail(tail(xs)))))))))=="t"  = obtenerDatos(tail(tail(tail(tail(tail(tail(tail(tail(tail(xs))))))))))
	| otherwise = ([],"error")
				
compruebaEtiquetaVersion (x:xs)
	| head (x:xs) == "<" && head (tail (x:xs)) == "?" && last (x:xs) == ">" && last (init (x:xs))=="?" = 1
	|(x:xs)==[] = 0
	|otherwise = 2
	
devuelveSinVersion (x:xs)	
	| snd(obtenerNombre (words( myShow (head(words(head (x:xs)))))))=="version" = (x:xs)
	| otherwise = devuelveSinVersion (xs)	
removeNon st = do
				let ca=[ c | c <- st, c `elem` ['a'..'z']]		
				if null ca
					then return  "0"
					else do return  "1"
					
isNum st = do
				let ca=[ c | c <- st, c `elem` ['0'..'9']]		
				if null ca
					then return  "0"
					else do return  "1"					
hoja :: [a] -> Arbol a 
hoja x = Nodo x []

myShow :: String -> String
myShow s = concat [ intersperse '\n' s]

pparsexml e= do
				contents <- readFile e 
				let ao = quitaEspacio(lines contents)
				let aoi = words(myShow (contents))
				print ao
				let x=parse0 ao
				let respuesta=(fst x)
				case respuesta of 
									1 -> return x--"El archivo es correcto"
									2 -> return x--"El archivo es incorrecto"
									0 -> return x--"no hay archivo"
					
main = do pparsexml "martes.xml"


{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import System.IO
import Data.List

 
--Se define una estructura llamada Arbol , la cual puede ser una Hoja o un Nodo con una lista como contenido y una lista de Arboles.
data Arbol a = Hoja | Nodo [a] [Arbol a ]	
			   deriving (Show, Eq)
			   
--Esta funcion recibe :
--Una lista y un arbol e inserta en el primer elemento 
--de la lista del arbol que se recibe ,un arbol con el contenido de la lista recibida.
insertarArbol e Hoja = Nodo e [Hoja]
insertarArbol e (Nodo x xs)
  | xs == [Hoja] = Nodo x (tail (xs ++ [Nodo e [Hoja]]))
  | xs==xs = Nodo x (xs ++ [Nodo e [Hoja]])
 
 --Esta funcion recibe :
--Dos Arboles ,e inserta el primer arbol como elemento de la lista de arboles del segundo Arbol
insertarNodoenArbol Hoja (Nodo x xs) = (Nodo x xs)
insertarNodoenArbol nodo (Nodo x xs)
  | xs == [Hoja] = Nodo x (tail (xs ++ [nodo]))
  | xs==xs = Nodo x (xs ++ [nodo])
  
--Esta funcion resive una lista con una etiqueta y comprueba 
--si es una etiqueta cerrada, el principio de una cadena abierta o el fin de esta 		 
compruebaEtiqueta0 []= 0					
compruebaEtiqueta0 (x:xs) 
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] && last (x:xs) == ">" && last (init (x:xs))=="/" 	=  1
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] &&  last (x:xs) == ">" && last (init (x:xs))=="\"" = 2
	| head (x:xs) == "<" && removeNon (head (tail (x:xs))) == ["1"] &&  last (x:xs) == ">" && removeNon (last (init (x:xs))) == ["1"] = 2
	| head (x:xs) == "<" && head (tail (x:xs)) == "/" &&  last (x:xs) == ">" && removeNon (last (init (x:xs))) == ["1"]  = 3
	|otherwise = 4
	
--Recibe una lista de lineas y le quita el espacio a cada una 
quitaEspacio x
	| x==[] = []
	| x==x =lines(unwords(words( unwords [head x]))) ++ quitaEspacio (tail x)
	
--Esta funcio recibe :
--Una lista que en el programa es la lista con los caracteres , comprueba si el archivo esta bien escrito 
--, realiza el parseo y guada los datos en un Arbol
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
				
--Recibe una lista y devuelve si el contenido es un xml pero sin la parte de la vesion 			
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
										 
								
					


--Recibe una lista y devuelve el primer nombre que encuentre
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

	 
--Recibe una lista y devuelve el primer nombre o string que encuentre que cumpla
-- en ser el valor de un atributo						 
obtenerDatos []=([],"")			
obtenerDatos(x:xs)
	 | x=="="&&head(xs)=="\""&& removeNon (head(tail(xs)))==["1"] = (fst (obtenerDatos(tail(tail(xs)))) ,head(tail (xs)) ++ snd(obtenerDatos(tail(tail(xs)))))
	 | x=="="&&head(xs)=="\""&& head(tail(xs))=="\"" = (tail(tail(xs)),"")
	 | x=="="&&head(xs)=="\""&& isNum (head(tail(xs)))==["1"] = (fst (obtenerDatos(tail(tail(xs)))) ,head(tail (xs)) ++ snd(obtenerDatos(tail(tail(xs)))))
	 | x=="\"" =((xs),"")
	 | otherwise = (fst(obtenerDatos(xs)) ,x ++ snd (obtenerDatos(xs)))	
	 
--Recibe una cadena y devuelve el nombre de un Capability 	 
obtenerNombreC []=([],"")								
obtenerNombreC (x:xs)
	 | x=="n" && head(xs) == "a"&& head(tail(xs))=="m" && head(tail(tail(xs))) == "e" = obtenerDatos(tail(tail(tail(xs))))--obtenerNombre (tail(xs))
	 | otherwise = ([],"error")	
	 
--Recibe una cadena y devuelve el value de un Capability 	  
obtenerValueC []=([],"")								
obtenerValueC (x:xs)
	 | x=="v" && head(xs) == "a"&& head(tail(xs))=="l" && head(tail(tail(xs))) == "u" && head(tail(tail(tail(xs))))=="e" = obtenerDatos(tail(tail(tail(tail(xs)))))--obtenerNombre (tail(xs))
   	 | otherwise = ([],"error")	
	 
--Recibe una cadena y devuelve el id de un Group	 
obtenerIdG []=([],"")								
obtenerIdG(x:xs)
	 | x=="i" && head(xs) == "d"  = obtenerDatos(tail(xs))
   	 | otherwise = ([],"error")	
	 
--Recibe una cadena y devuelve el id de un Device 	 
obtenerIdD []=([],"")								
obtenerIdD(x:xs)
	 | x=="i" && head(xs) == "d"  = obtenerDatos(tail(xs))
   	 | otherwise = ([],"error")		
	 
--Recibe una cadena y devuelve el FallBack de un Device	 
obtenercD []=([],"")								
obtenerFallBackD (x:xs)
	 | x=="f" && head(xs) == "a"&& head(tail(xs))=="l" && head(tail(tail(xs))) == "l" && head(tail(tail(tail(xs))))=="_" && head(tail(tail(tail(tail(xs)))))=="b" && head(tail(tail(tail(tail(tail(xs))))))=="a" && head(tail(tail(tail(tail(tail(tail(xs)))))))=="c" && head(tail(tail(tail(tail(tail(tail(tail(xs))))))))=="k"  = obtenerDatos(tail(tail(tail(tail(tail(tail(tail(tail(xs)))))))))
	 | otherwise = ([],"error")	
	 
--Recibe una cadena y devuelve el UserAgent de un Device	 	 
obtenerUserAgentG []=([],"")								
obtenerUserAgentG (x:xs)
	| x=="u" && head(xs) == "s"&& head(tail(xs))=="e" && head(tail(tail(xs))) == "r" && head(tail(tail(tail(xs))))=="_" && head(tail(tail(tail(tail(xs)))))=="a" && head(tail(tail(tail(tail(tail(xs))))))=="g" && head(tail(tail(tail(tail(tail(tail(xs)))))))=="e" && head(tail(tail(tail(tail(tail(tail(tail(xs))))))))=="n" && head(tail(tail(tail(tail(tail(tail(tail(tail(xs)))))))))=="t"  = obtenerDatos(tail(tail(tail(tail(tail(tail(tail(tail(tail(xs))))))))))
	| otherwise = ([],"error")
				
--Esta funcion recibe una lista y comprueba si comienza como una version de xml				
compruebaEtiquetaVersion (x:xs)
	| head (x:xs) == "<" && head (tail (x:xs)) == "?" && last (x:xs) == ">" && last (init (x:xs))=="?" = 1
	|(x:xs)==[] = 0
	|otherwise = 2
	
	
--Resive una lista en la cual esta parte del xml y 
--devuelve otra lista pero sin la etiqueta de evrsion de cierre
devuelveSinVersion (x:xs)	
	| snd(obtenerNombre (words( myShow (head(words(head (x:xs)))))))=="version" = (x:xs)
	| otherwise = devuelveSinVersion (xs)	
	
--Esta funcion recibe un Char y devuelve si es letra minuscula	
removeNon st = do
				let ca=[ c | c <- st, c `elem` ['a'..'z']]		
				if null ca
					then return  "0"
					else do return  "1"
	
--Esta funcion recibe un Char y devuelve si es numero	
isNum st = do
				let ca=[ c | c <- st, c `elem` ['0'..'9']]		
				if null ca
					then return  "0"
					else do return  "1"					


--Esta funcion recibe un String y retorna el mismo 
--string pero con un salto de linea por cada letra 
myShow :: String -> String
myShow s = concat [ intersperse '\n' s]

--Esta funcion recibe el archivo xml y retora 1 si esta correcto, 
--2 si esta incorecto y cero si el archivo estaba vacio
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
									
									
--El main que llama a la funcion de parseo con el archivo a revisar					
main = do pparsexml "martes.xml"


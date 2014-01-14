{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
-- import Text.XML.HXT.Core
import System.IO
import Data.List
import Data.Char
import Data.String

recorerrLineas3 :: ([String] , Int) -> IO()
recorerrLineas3 ([], i) = print "FIN"
recorerrLineas3 ((x:xs), i) = do
    print x
    recorerrLineas3 (xs,i+1)

obtenerChar :: (String,Integer,Integer) -> Char
obtenerChar ([],pos,i) = ' '
obtenerChar ((x:xs), pos, i) = 
	if i == pos
		then x
		else obtenerChar (xs, pos, i+1)

buscarHasta :: (String , Char , Integer , Integer) -> Integer
buscarHasta ([], bus, ini, i) = (-1) -- SI no lo encuentra
buscarHasta ((x:xs), bus, ini, i) = 
	if i < ini
		then buscarHasta (xs, bus, ini, i+1)
		else
			if x==bus 
				then i
				else buscarHasta (xs, bus, ini, i+1)

extraerInfoGrupo :: (String,Integer,Integer,String) -> String
extraerInfoGrupo ([],ini,i,detalle)=detalle
extraerInfoGrupo ((x:xs),ini,i,detalle)=
	if i < ini 
		then extraerInfoGrupo (xs,ini,i+1,detalle)
		else
			if x=='>'
				then detalle
				else extraerInfoGrupo (xs,ini,i+1,detalle++[x])
				
extraerDetalle :: (String,Integer,Integer,String) -> String
extraerDetalle ([],ini,i,detalle)=detalle
extraerDetalle ((x:xs),ini,i,detalle)=
	if i < ini 
		then extraerDetalle (xs,ini,i+1,detalle)
		else
			if x=='/'
				then detalle
				else extraerDetalle (xs,ini,i+1,detalle++[x])
				

extraerNombre :: (String,Integer,Integer,String) -> String
extraerNombre ([],ini,i,nombre)=nombre
extraerNombre ((x:xs),ini,i,nombre)=
	if i < ini 
		then extraerNombre (xs,ini,i+1,nombre)
		else
			if x==' ' || x=='>'
				then nombre
				else extraerNombre (xs,ini,i+1,nombre++[x])
				
				
buscarGrupo :: ([String] , String , String) -> Integer
buscarGrupo ([], buscar,name) = 0
buscarGrupo ((x:xs), buscar,name) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[]);
		det = extraerInfoGrupo (x,pos + 7,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then
						if det == "id=\""++name++"\"" 
							then 1
							else buscarGrupo (xs, buscar,name) --busca demas lineas
					else
						if nom == "device" then 0
						else buscarGrupo (xs, buscar,name)
			else buscarGrupo (xs, buscar,name)
					

buscarCapacidad :: ([String] , String , String,String) -> Integer
buscarCapacidad ([], buscar,name,value) = 0
buscarCapacidad ((x:xs), buscar,name,value) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[]);
		det = extraerDetalle (x,pos + 12,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then
						if det == "name=\""++name++"\" value=\""++value++"\"" 
							then 1
							else buscarCapacidad (xs, buscar,name,value) --busca demas lineas
					else
						if nom == "device" then 0
						else buscarCapacidad (xs, buscar,name,value)
			else buscarCapacidad (xs, buscar,name,value)
					
recorerrLineas :: ([String] , String , Integer,String,String) -> IO()
recorerrLineas ([], buscar ,i,name,value) = print (buscar++" soportan ("++name++"="++value++"): "  ++ show i)
recorerrLineas ((x:xs), buscar,i,name,value) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then 
						if (buscarCapacidad (xs, "capability",name,value))==1
							then do
								print x
								recorerrLineas (xs, buscar, i+1,name,value)
							else
								recorerrLineas (xs, buscar, i,name,value)
					else recorerrLineas (xs, buscar, i,name,value)
			else recorerrLineas (xs, buscar, i,name,value)
			
			
recorerrLineas2 :: ([String] , String , Integer) -> IO()			
recorerrLineas2 ([], buscar ,i) = print (" Fin "  ++ show i)
recorerrLineas2 ((x:xs), buscar,i) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then do
							print x
							recorerrLineas2 (xs, buscar, i+1)
				else
						recorerrLineas2 (xs, buscar, i)
					
			else recorerrLineas2 (xs, buscar, i)
			
	
recorerrLineas4 :: ([String] , String , Integer,String) -> IO()
recorerrLineas4 ([], buscar ,i,name) = print (buscar++" soportan ("++name++"): "  ++ show i)
recorerrLineas4 ((x:xs), buscar,i,name) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then 
						if (buscarGrupo(xs, "group",name))==1
							then do
								print x
								recorerrLineas4 (xs, buscar, i+1,name)
							else
								recorerrLineas4 (xs, buscar, i,name)
					else recorerrLineas4 (xs, buscar, i,name)
			else recorerrLineas4 (xs, buscar, i,name)	

	
pparsexml e = do
				contents <- readFile e 
				let ao = lines contents
				--recorerrLineas2 (ao, "capability", 0)
				--recorerrLineas3 (ao,0)
				recorerrLineas (ao, "device", 0,"cookie_support","true") --busca dispositivo con tal capacidad
				--recorerrLineas4 (ao, "device", 0,"ajax")
				--return ao
				
				
main = do 
			pparsexml "wurfl-2.3.xml"
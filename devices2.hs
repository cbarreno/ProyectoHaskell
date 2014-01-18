{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
-- import Text.XML.HXT.Core
import System.IO
import Data.List
import Data.Char
import Data.String

--Función imprimirXml: Dado un String de lineas, imprime por pantalla cada una de las líneas del xml
imprimirXml :: ([String] , Int) -> IO()
imprimirXml ([], i) = print "FIN"
imprimirXml ((x:xs), i) = do
    print x
    imprimirXml (xs,i+1)

--Función obtenerChar: Dado una linea en específico,una posición y un contador; retorna  un caracter
obtenerChar :: (String,Integer,Integer) -> Char
obtenerChar ([],pos,i) = ' '
obtenerChar ((x:xs), pos, i) = 
	if i == pos
		then x
		else obtenerChar (xs, pos, i+1)

		
--Función buscarHasta: Dado una línea en específico, un char a buscar, un inicio y i (un contador o acumulador); retorna i que es la posición del Caracter enviado 
buscarHasta :: (String , Char , Integer , Integer) -> Integer
buscarHasta ([], bus, ini, i) = (-1) -- SI no lo encuentra
buscarHasta ((x:xs), bus, ini, i) = 
	if i < ini
		then buscarHasta (xs, bus, ini, i+1)
		else
			if x==bus 
				then i
				else buscarHasta (xs, bus, ini, i+1)


--Función extraerInfoGrupo: Dado una línea con la posición del char específica, i un contador y un string vacío; va retornar el mismo string concatenada hasta antes del char '>' que en este caso seria el id del grupo			
--Ej: id="product_info"
extraerInfoGrupo :: (String,Integer,Integer,String) -> String
extraerInfoGrupo ([],ini,i,detalle)=detalle
extraerInfoGrupo ((x:xs),ini,i,detalle)=
	if i < ini 
		then extraerInfoGrupo (xs,ini,i+1,detalle)
		else
			if x=='>'
				then detalle
				else extraerInfoGrupo (xs,ini,i+1,detalle++[x])

--Función extraerInfoDevice: Dado una línea con la posición del char específica, i un contador y un string vacío; va retornar el mismo string pero concatenado hasta antes del char '>' que en este caso sería la info del device
--Ej: id="generic" user_agent="" fall_back="root"					
extraerInfoDevice :: (String,Integer,Integer,String) -> String
extraerInfoDevice ([],ini,i,detalle)=detalle
extraerInfoDevice ((x:xs),ini,i,detalle)=
	if i < ini 
		then extraerInfoDevice (xs,ini,i+1,detalle)
		else
			if x=='>'
				then detalle
				else extraerInfoDevice (xs,ini,i+1,detalle++[x])				
				
--Función extraerDetalleCapab: Dado una línea con la posición del char específica, i un contador y un string vacío; va retornar el mismo string pero concatenado hasta antes del char '/' que en este caso sería la info del capability
--Ej: name="icons_on_menu_items_support" value="false"				
extraerDetalleCapab :: (String,Integer,Integer,String) -> String
extraerDetalleCapab ([],ini,i,detalle)=detalle
extraerDetalleCapab ((x:xs),ini,i,detalle)=
	if i < ini 
		then extraerDetalleCapab (xs,ini,i+1,detalle)
		else
			if x=='/'
				then detalle
				else extraerDetalleCapab (xs,ini,i+1,detalle++[x])
				

				
--Función extraerNombre: Dado una línea con la posición del char específica, i un contador y un string vacío; va retornar el mismo string pero concatenado hasta antes del char ' ' o '>' que en este caso sería el nombre de la etiqueta: device, group o capability segun lo que se le envie
extraerNombre :: (String,Integer,Integer,String) -> String
extraerNombre ([],ini,i,nombre)=nombre
extraerNombre ((x:xs),ini,i,nombre)=
	if i < ini 
		then extraerNombre (xs,ini,i+1,nombre)
		else
			if x==' ' || x=='>'
				then nombre
				else extraerNombre (xs,ini,i+1,nombre++[x])
				
	
--Función buscarDevice: Dado una lista de strings(un device en especifico), el tag device y sus atributos id, user_agent y fall_back
--Retorna 1 si lo encontró, caso contrario 0
buscarDevice :: ([String] , String , String, String, String) -> Integer
buscarDevice ([], buscar,id,user_agent,fall_back) = 0
buscarDevice ((x:xs), buscar,id,user_agent, fall_back) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[]);
		det = extraerInfoDevice (x,pos + 8,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar 
					then
						if det == "id=\""++id++"\" user_agent=\""++user_agent++"\" fall_back=\""++fall_back++"\"" 
							then 1
							else buscarDevice (xs, buscar,id,user_agent,fall_back) --busca demas lineas
					else
						if nom == "device" then 0
						else buscarDevice (xs, buscar,id,user_agent,fall_back)
			else buscarDevice(xs, buscar,id,user_agent,fall_back)
			

--Función buscarGrupo: Dado	una lista de lineas, el tag group y el nombre del grupo
--Retorna 1 si lo encontró, caso contrario 0
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
							else buscarGrupo (xs, buscar,name) --busca demás lineas
					else
						if nom == "device" then 0
						else buscarGrupo (xs, buscar,name)
			else buscarGrupo (xs, buscar,name)
					

--Función buscarCapacidad: Dado	una lista de lineas, el tag capability y sus atributos name y value
--Retorna 1 si lo encontró, caso contrario 0
--Observación: La función recorrerCapacidadDevice encuentra un Tag Device en una línea específica, luego le envia a buscarCapacidad la cola(las demas líneas del xml a partir de ese tag device)
--Y si encuentra otro tag device retorna 0; quiere decir que buscarCapacidad solo va buscar solo las capacidades de ese tag device
buscarCapacidad :: ([String] , String , String,String) -> Integer
buscarCapacidad ([], buscar,name,value) = 0
buscarCapacidad ((x:xs), buscar,name,value) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[]);
		det = extraerDetalleCapab (x,pos + 12,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then
						if det == "name=\""++name++"\" value=\""++value++"\"" 
							then 1
							else buscarCapacidad (xs, buscar,name,value) --busca demas lineas
					else
						if nom == "device" then 0 --condición si encuentra otro device retorna 0
						else buscarCapacidad (xs, buscar,name,value)
			else buscarCapacidad (xs, buscar,name,value)
					

					
--Función buscarCapacidad2: Dado una lista de lineas, el tag capability y sus atributos name y value, imprime todos las capacidades de un device en específico		
buscarCapacidad2 :: ([String] , String , Integer) -> IO()			
buscarCapacidad2 ([], buscar ,i) = print (" Fin "  ++ show i)
buscarCapacidad2 ((x:xs), buscar,i) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then do
								print x
								buscarCapacidad2 (xs, buscar, i+1)
					else
						if(nom=="device") then
							print ""
					else buscarCapacidad2 (xs, buscar, i)	
				else buscarCapacidad2 (xs, buscar, i)			
			

--Función recorrerCapacidadDevice: Dado una lista de lineas(strings), 1 String(device), 1 contador i y una capacidad específica(name y value)
--Imprime todos los devices que tienen dicha capacidad y también imprime cuántos encontró			
recorrerCapacidadDevice :: ([String] , String , Integer,String,String) -> IO()
recorrerCapacidadDevice([], buscar ,i,name,value) = print (buscar++" soportan ("++name++"="++value++"): "  ++ show i)
recorrerCapacidadDevice((x:xs), buscar,i,name,value) = 
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
								recorrerCapacidadDevice(xs, buscar, i+1,name,value)
							else
								recorrerCapacidadDevice(xs, buscar, i,name,value)
					else recorrerCapacidadDevice(xs, buscar, i,name,value)
			else recorrerCapacidadDevice(xs, buscar, i,name,value)
			
			
			
--Función recorrerDevices: Dado una lista de lineas(strings), 1 String(device), 1 contador i y los atributos específicos de 1 device(id,user_agent,fall_back)
--Si lo encuentra gracias a la funcion BuscarDevice, lo imprime indicando 1 si lo encontró y 0 si no lo encontró					
recorrerDevices :: ([String] , String , Integer,String,String,String) -> IO()
recorrerDevices ([], buscar ,i,id,user_agent,fall_back) = print (i)
recorrerDevices ((x:xs), buscar,i,id,user_agent,fall_back) =  
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then 
						if (buscarDevice ([x], "device",id,user_agent,fall_back)==1)
							then do
								print x
								recorrerDevices (xs, buscar, i+1,id,user_agent,fall_back)
							else
								recorrerDevices (xs, buscar, i,id,user_agent,fall_back)
					else recorrerDevices (xs, buscar, i,id,user_agent,fall_back)
			else recorrerDevices (xs, buscar, i,id,user_agent,fall_back)			
			

			
--Función recorrerDevices2: Dado una lista de lineas(strings), 1 String(device), 1 contador i y los atributos específicos de 1 device(id,user_agent,fall_back)
--Si lo encuentra imprime todas las capacidades de ese device y cuántas encontró			
recorrerDevices2 :: ([String] , String , Integer,String,String,String) -> IO()
recorrerDevices2 ([], buscar ,i,id,user_agent,fall_back) = print (i)
recorrerDevices2 ((x:xs), buscar,i,id,user_agent,fall_back) =  
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then 
						if (buscarDevice ([x], "device",id,user_agent,fall_back)==1)
							then do
								    buscarCapacidad2(xs,"capability",0)
									
								else
									recorrerDevices2 (xs, buscar, i,id,user_agent,fall_back)
						else recorrerDevices2 (xs, buscar, i,id,user_agent,fall_back)
				else recorrerDevices2 (xs, buscar, i,id,user_agent,fall_back)	
		
		

--Funcion recorrerLineas: Dado un lista de lineas del xml, un String(tag a buscar:group, device o capability), i un contador
--Esta función va recorrer y imprimir todos los devices, grupos o capacidades indicando cuántos encontró
recorrerLineas :: ([String] , String , Integer) -> IO()			
recorrerLineas ([], buscar ,i) = print (" Fin "  ++ show i)
recorrerLineas ((x:xs), buscar,i) = 
	let pos = buscarHasta (x, '<', 0, 0);
		c = obtenerChar (x, pos+1, 0);
		nom = extraerNombre (x,pos+1,0,[])
	in
		if c /= '/'
			then 
				if nom == buscar
					then do
							print x
							recorrerLineas (xs, buscar, i+1)
				else
						recorrerLineas (xs, buscar, i)
					
			else recorrerLineas (xs, buscar, i)
			
			

--Función recorrerGroupDevice: Dado una lista de lineas(strings), 1 String(device), 1 contador i y un String(grupo en específico)
--Retorna los devices que pertenecen al grupo indicado y cuántos encontró
recorrerGroupDevice :: ([String] , String , Integer,String) -> IO()
recorrerGroupDevice ([], buscar ,i,name) = print (buscar++" soportan ("++name++"): "  ++ show i)
recorrerGroupDevice ((x:xs), buscar,i,name) = 
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
								recorrerGroupDevice (xs, buscar, i+1,name)
							else
								recorrerGroupDevice (xs, buscar, i,name)
					else recorrerGroupDevice (xs, buscar, i,name)
			else recorrerGroupDevice (xs, buscar, i,name)	

			

--Funcion pparsexml: Lee el archivo xml, lo cual se lo asigna a contents; luego la variable ao guarda todo el contenido de las lineas del xml. 
--Finalmente se llama a las funciones ya implementas ya sea: imprimirXml, recorrerLineas, recorrerCapacidadDevice, recorrerGroupDevice o recorrerDevices2.
--return ao  imprime todo el archivo xml 	
parsexml e = do
				contents <- readFile e 
				let ao = lines contents
				--recorrerLineas (ao, "device", 0)
				--imprimirXml (ao,0)
				recorrerCapacidadDevice(ao, "device", 0,"max_image_width","90") --busca dispositivo o grupo con tal capacidad
				--recorrerGroupDevice (ao, "device", 0,"ajax")
				--recorrerDevices2 (ao, "device",0,"generic_mobile","DO_NOT_MATCH_GENERIC_MOBILE","generic")
				--return ao
				
				
				
--Función principal main que llama a parsexml				
main = do 
			parsexml "wurfl-2.3.xml"
			
			
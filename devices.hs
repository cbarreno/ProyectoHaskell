{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Text.XML.HXT.Core
import System.IO
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
 
parseXML file = readDocument [ withValidate no
                             , withRemoveWS yes  -- throw away formating WS
                             ] file
 
atTag tag = deep (isElem >>> hasName tag)
  
  

pparsexml f= do
					archivo <- openFile f ReadMode 
					contenidoArchivo <- hGetContents archivo 
					putStr contenidoArchivo
					hClose archivo
				
 
-- Incremental development of the getTeams function:
 
-- Try capturing some statistics about the players
 
significant = not . all (`elem` " \n\r\t")
 
-- Use our definition of "significant" strings to
-- capture the value; or else nothing.
 
getStat attr = 
  (getAttrValue attr >>> isA significant >>> arr Just)
    `orElse` (constA Nothing)

getCapabilities = atTag "capability" >>>
  proc c -> do
    name    <- getAttrValue "name" -< c
    value    <- getAttrValue "value"    -< c
    returnA -< Capability
      { name = name,
        value = value}

getGroups = atTag "group" >>>
  proc p -> do
    id    <- getAttrValue "id" -< p
    capabilitys  <- listA getCapabilities  -< p
    returnA -< Group
      { idG = id,
        capabilitys  = capabilitys}
	 
getDevices = atTag "device" >>>
  proc l -> do
    id <- getAttrValue "id" -< l
    userAgent <- getAttrValue "user_agent" -< l
    fallBack <- getAttrValue "fall_back" -< l
    groups  <- listA getGroups    -< l
    returnA -< Device
      { idD   = id,
        userAgent = userAgent,
        fallBack = fallBack,
        groups     = groups}
 
-- Our final choices
 
-- main = do
  -- devices <- runX (parseXML "jueves.xml" >>> getDevices)
  -- print devices
  
  -- main = do
			-- archivo <- openFile "jueves.xml" ReadMode 
			-- contenidoArchivo <- hGetContents archivo 
			-- putStr contenidoArchivo
			-- hClose archivo
		 
module Library where
import PdePreludat

{-
Nombre: Apellido, Nombre
Legajo: 999999-9
-}

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Number,
  precisionJugador :: Number
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Number,
  precision :: Number,
  altura :: Number
} deriving (Eq, Show)

type Puntos = Number

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

type Palo = Habilidad -> Tiro

putter habilidad = UnTiro {
    velocidad = 10,
    precision = (*2).precisionJugador $ habilidad,
    altura = 0
} 


madera habilidad = UnTiro {
    velocidad = 100,
    precision = (/2).precisionJugador $ habilidad,
    altura = 5
} 

minimoCero n 
    | n>=3 = n-3
    | otherwise = 0

hierros n  habilidad = UnTiro {
    velocidad = fuerzaJugador habilidad * n,
    precision = (*n).precisionJugador $ habilidad,
    altura = minimoCero n
} 

palos :: [Palo]
palos = [ putter, madera, (hierros 1), (hierros 2), (hierros 3), (hierros 4), (hierros 5), (hierros 6), (hierros 7), (hierros 8), (hierros 9), (hierros 10)]

golpe persona palo = palo habilidad persona

type Obstaculo = Tiro -> Tiro
pasaObstaculo tiro 
    | (precision tiro > 90) && (altura tiro == 0) = True
    | (velocidad tiro > 80) && ((altura tiro <= 5) && (altura tiro >= 1)) = True
    | (&&) ((velocidad tiro >=5) && (velocidad tiro <=20)) ((altura tiro == 0) && (precision tiro > 95)) = True 
    | otherwise = False

tunel tiro
    | pasaObstaculo tiro = UnTiro (velocidad tiro * 2) 100 0
    | otherwise = UnTiro 0 0 0  

laguna largo tiro 
    | pasaObstaculo tiro = tiro {altura = (altura tiro) / largo}
    | otherwise = UnTiro 0 0 0 

hoyo tiro 
    | pasaObstaculo tiro  = UnTiro 0 0 0 
    | otherwise = UnTiro 0 0 0 

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles persona obstaculo = filter (pasaObstaculo . (flip ($) (habilidad persona))) palos

obstaculosConsecutivos :: Tiro -> [Obstaculo] -> Number
obstaculosConsecutivos tiro = length . (takeWhile (pasaObstaculo . flip ($) tiro))

--paloMasUtil persona obstaculos = foldl () 0 palos
--  where utiles = map ((palosUtiles persona)) obstaculos

--paloMasUtil persona obstaculos= map (maximoSegun obstaculosConsecutivos). (map ($ persona)) . map (palosUtiles persona) $ obstaculos

paloMasUtil persona obstaculos= maximoSegun ((flip obstaculosConsecutivos obstaculos) . ($ (habilidad persona)))  palos

--padresPerdedores resultados = .map (snd)
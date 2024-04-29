module Lib () where

ejemplo :: Tablero
ejemplo = Tablero (3,3) [Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0),Celda (0,0,0,0), Celda (0,0,0,0)] (Cabezal (1,1))

--Punto 1: Modelar
type Tamaño = (Int, Int)

type Coordenada = Tamaño

type Bolitas = (Int, Int, Int, Int) --Rojo, azul, verde, negro

data Color = Rojo | Azul | Verde | Negro deriving (Eq)

data Direccion = Norte | Sur | Este | Oeste

data Tablero = Tablero {
  tamaño  :: Tamaño,
  celda   :: [Celda],
  cabezal :: Cabezal
  }deriving (Show)

data Cabezal = Cabezal Coordenada deriving (Show)

data Celda = Celda Bolitas deriving (Show)

 --Punto 2: Inicializar Tablero
inicializarTablero :: Int -> Int -> Tablero
inicializarTablero filas columnas = Tablero (filas, columnas) (crearTablero (filas*columnas)) (Cabezal (1, 1)) 

crearTablero :: Int -> [Celda]
crearTablero cantidadDeCelda = map (\n -> Celda (0,0,0,0)) [1..cantidadDeCelda]

--Punto 3: 
--Mover
mover :: Direccion -> Sentencia
mover direccion tablero
  |puedeMoverse direccion tablero = tablero {cabezal = moverCabezal (cabezal tablero) direccion}
  |otherwise = error "El cabezal se cayó del tablero"

coordenada :: Cabezal -> Coordenada
coordenada (Cabezal coords) = coords

sumarTuplas :: (Num a) => (a, a) -> (a, a) -> (a, a)
sumarTuplas (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

verificarExistenciaDeCelda :: Tamaño -> Coordenada -> Bool
verificarExistenciaDeCelda tamaño coord = fst coord <= fst tamaño && snd coord <= snd tamaño && fst coord /= 0 && snd coord /= 0

puedeMoverse :: Direccion -> Condicion
puedeMoverse direccion tablero = verificarExistenciaDeCelda (tamaño tablero) (coordenada (moverCabezal (cabezal tablero) direccion))

moverCabezal :: Cabezal -> Direccion -> Cabezal
moverCabezal cabezal (Norte) = Cabezal (sumarTuplas (coordenada cabezal) (1,0))
moverCabezal cabezal (Sur)   = Cabezal (sumarTuplas (coordenada cabezal) ((-1),0))
moverCabezal cabezal (Este)  = Cabezal (sumarTuplas (coordenada cabezal) (0,1))
moverCabezal cabezal (Oeste) = Cabezal (sumarTuplas (coordenada cabezal) (0,(-1)))

--Poner
poner ::  Color -> Sentencia
poner color tablero = tablero {celda = nuevaListaConBolita (celda tablero) color (indiceDeCabezal tablero)}

nuevaListaConBolita :: [Celda] -> Color -> Int -> [Celda]
nuevaListaConBolita celda color indice = take indice celda ++ (agregarBolitaACelda ((!!) celda indice) color : drop (indice + 1) celda) 

agregarBolitaACelda :: Celda -> Color -> Celda
agregarBolitaACelda celda (Rojo)  = Celda (sumarBolita (bolitas celda) (1,0,0,0))
agregarBolitaACelda celda (Azul)  = Celda (sumarBolita (bolitas celda) (0,1,0,0))
agregarBolitaACelda celda (Verde) = Celda (sumarBolita (bolitas celda) (0,0,1,0))
agregarBolitaACelda celda (Negro) = Celda (sumarBolita (bolitas celda) (0,0,0,1))

indiceDeCabezal :: Tablero -> Int
indiceDeCabezal tablero = (snd . tamaño) tablero * ((+(-1))  . fst . coordenada . cabezal) tablero + ((+(-1)) . snd . coordenada . cabezal) tablero

bolitas :: Celda -> Bolitas
bolitas (Celda bolitas) = bolitas

sumarBolita :: (Num a) => (a, a, a, a) -> (a, a, a, a) -> (a, a, a, a)
sumarBolita (a, b, c, d) (e, f, g, h) = (a + e, b + f, c + g, d + h)

--Sacar
sacar :: Color -> Sentencia
sacar color tablero
  |hayBolita color tablero = tablero {celda = nuevaListaSinBolita (celda tablero) color (indiceDeCabezal tablero)}
  |otherwise = error ("No hay bolitas del color " ++ stringDeColor color ++ " para sacar de la celda actual")

stringDeColor :: Color -> String
stringDeColor Rojo  = "rojo"
stringDeColor Azul  = "azul"
stringDeColor Verde = "verde"
stringDeColor Negro = "negro"

hayBolita :: Color -> Condicion
hayBolita color tablero = not $ hayAlgunNegativo $ bolitas $ sacarBolitaACelda ((!!) (celda tablero) (indiceDeCabezal tablero)) color

hayAlgunNegativo :: Bolitas -> Bool
hayAlgunNegativo (a,b,c,d) = a == -1 || b == -1 || c == -1 || d == -1 

nuevaListaSinBolita :: [Celda] -> Color -> Int -> [Celda]
nuevaListaSinBolita celda color indice = take indice celda ++ (sacarBolitaACelda ((!!) celda indice) color : drop (indice + 1) celda) 

sacarBolitaACelda :: Celda -> Color -> Celda
sacarBolitaACelda celda (Rojo)  = Celda (sumarBolita (bolitas celda) (-1,0,0,0))
sacarBolitaACelda celda (Azul)  = Celda (sumarBolita (bolitas celda) (0,-1,0,0))
sacarBolitaACelda celda (Verde) = Celda (sumarBolita (bolitas celda) (0,0,-1,0))
sacarBolitaACelda celda (Negro) = Celda (sumarBolita (bolitas celda) (0,0,0,-1))

type Sentencia = Tablero -> Tablero

--Punto 4
--Repetir
repetir :: Int -> [Sentencia] -> Tablero -> Tablero
repetir cantidad sentencias = aplicarSentenciasATablero (concat (replicate cantidad sentencias))

--Alternativa
type Condicion = (Tablero -> Bool)

alternativa :: Condicion -> [Sentencia] -> [Sentencia] -> Tablero -> Tablero
alternativa condicion conjuntoA conjuntoB tablero 
  |condicion tablero = aplicarSentenciasATablero conjuntoA tablero
  |otherwise         = aplicarSentenciasATablero conjuntoB tablero
 
si :: Condicion -> [Sentencia] -> Tablero -> Tablero
si condicion conjunto tablero
  |condicion tablero = aplicarSentenciasATablero conjunto tablero
  |otherwise         = tablero 

siNo :: Condicion -> [Sentencia] -> Tablero -> Tablero
siNo condicion conjunto tablero
  |condicion tablero = tablero
  |otherwise         = aplicarSentenciasATablero conjunto tablero

aplicarSentenciasATablero :: [Sentencia] -> Tablero -> Tablero
aplicarSentenciasATablero conjunto = foldr (.) id (reverse conjunto)

--Mientras 
mientras :: Condicion -> [Sentencia] -> Tablero -> Tablero
mientras condicion sentencias tablero
  |condicion tablero = mientras condicion sentencias (aplicarSentenciasATablero sentencias tablero)
  |otherwise = tablero

--Ir al borde
irAlBorde :: Direccion -> Tablero -> Tablero
irAlBorde direccion = mientras (puedeMoverse direccion) [mover direccion]

--Punto 5

--Puede moverse (hecha más arriba)
--Hay bolita (hecha más arriba)

--Cantidad de bolitas
cantidadDeBolitas :: Color -> Tablero -> Int
cantidadDeBolitas color tablero = cantidadBolitasEnTupla (bolitas ((!!) (celda tablero) (indiceDeCabezal tablero))) color

cantidadBolitasEnTupla :: Bolitas -> Color -> Int
cantidadBolitasEnTupla (rojo, azul, verde, negro) color
  |color == Rojo  = rojo
  |color == Azul  = azul
  |color == Verde = verde 
  |color == Negro = negro

--Punto 6
--Programa
programa :: Tablero -> [Sentencia] -> Tablero
programa tablero conjunto = aplicarSentenciasATablero conjunto tablero

--punto 7
punto7 = programa ejemplo [mover Norte, poner Negro, poner Negro, poner Azul, mover Norte, repetir 15 [poner Rojo, poner Azul], alternativa (hayBolita Verde) [mover Este, poner Negro] [mover Sur, mover Este, poner Azul], mover Este, mientras ((<=9) . cantidadDeBolitas Verde) [poner Verde], poner Azul]
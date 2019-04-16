import Text.Show.Functions

data Pirata = UnPirata{
    nombre :: String,
    botin :: [Tesoro]}
    deriving (Show,Eq)


type Barco = ([Pirata],FormaSaqueo)

type FormaSaqueo = (Tesoro -> Bool)

type Isla = (String,Tesoro)

type Ciudad = (String,[Tesoro])


--PIRATAS

jackSparrow = UnPirata "Jack Sparrow" [brujula,("Frasco De Arena",0)]
davidJones = UnPirata "David Jones" [cajitalMusical]
anneBonny = UnPirata "Anne Bonny" [doblones,("Frasco De Arena",1)]
elizabethSwann = UnPirata "Elizabeth Swann" [monedaCofreMuerto,espadaHierro]
willTurner = UnPirata "Will Turner" [cuchillo]


--TESOROS

type Tesoro = (String, Int)

brujula :: Tesoro
brujula = ("Brujula",10000)
cajitalMusical :: Tesoro
cajitalMusical = ("Cajita Musical",1)
doblones :: Tesoro
doblones = ("Doblones",100)
monedaCofreMuerto = ("Moneda Cofre Muerto", 100)
espadaHierro :: Tesoro
espadaHierro = ("Espada de Hierro", 50)
cuchillo :: Tesoro
cuchillo = ("Cuchillo", 5)
mapa :: Tesoro
mapa = ("Mapa",10)
botellaRon :: Tesoro
botellaRon = ("Botella de Ron",25)
oro :: Tesoro
oro = ("Oro",100)
sombrero :: Tesoro
sombrero = ("Sombrero",23)
frasco :: Tesoro
frasco = ("Frasco De Arena",1)

--BARCOS

perlaNegra = ([jackSparrow,anneBonny],saquearValioso)
holandesErrante = ([davidJones],saquearConCorazon)

--ISLAS

islaTortuga = ("Isla Tortuga", frasco)
islaRon = ("Isla del Ron", botellaRon)


-- CIUDADES
portRoyal = ("Port Royal", [brujula,doblones,cajitalMusical,monedaCofreMuerto])
carmenPatagones = ("Carmen de Patagones", [mapa])


---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------


cantTesoros :: Pirata -> Int
cantTesoros unPirata = length (botin unPirata)

esAfortunado :: Pirata -> Bool
esAfortunado unPirata = sum (map snd (botin unPirata)) > 10000

esMismoTesoroConValorDistinto :: Tesoro -> Tesoro -> Bool 
esMismoTesoroConValorDistinto unTesoro otroTesoro = (fst unTesoro == fst otroTesoro) && (snd unTesoro /= snd otroTesoro)

loTieneOtroPirataConDistintoValor :: Pirata -> Tesoro -> Bool
loTieneOtroPirataConDistintoValor unPirata unTesoro = any (esMismoTesoroConValorDistinto unTesoro) (botin unPirata)

tienenElMismoTesoroConValorDiferente :: Pirata -> Pirata -> Bool
tienenElMismoTesoroConValorDiferente  unPirata otroPirata = any (loTieneOtroPirataConDistintoValor otroPirata) (botin unPirata)

valorTesoroMasValioso :: Pirata -> Int
valorTesoroMasValioso unPirata = maximum (map snd (botin unPirata))

agregarUnTesoro :: Pirata -> Tesoro -> [Tesoro]
agregarUnTesoro unPirata unTesoro = unTesoro : (botin unPirata)
--Si uso el ++ en consola tendria que escribir [("Espada",35)], usando el ":" solo tengo que escribir el tesoro ej.: brujula 

esValioso :: Tesoro -> Bool
esValioso unTesoro = (snd unTesoro) > 100

pierdeTodosSusTesorosValiosos :: Pirata -> [Tesoro]
pierdeTodosSusTesorosValiosos unPirata = filter (not.esValioso) (botin unPirata)

pierdeUnTesoro :: Pirata -> String -> [String]
pierdeUnTesoro unPirata nombreTesoro = filter ((/=)nombreTesoro) (map fst (botin unPirata))
--pierdeUnTesoro unPirata unTesoro = filter (/= unTesoro) (botin unPirata)  --> Seria perder el tesoro entero


---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------


--TIPOS DE SAQUEOS

saquearValioso :: Tesoro -> Bool
saquearValioso unTesoro = esValioso unTesoro

saquearObjetoEspecifico :: Tesoro -> Tesoro -> Bool
saquearObjetoEspecifico unObjeto unTesoro = unObjeto == unTesoro

saquearConCorazon :: Tesoro -> Bool
saquearConCorazon unTesoro = False

saquearConjuncion :: Tesoro -> Tesoro -> Bool
saquearConjuncion unObjeto unTesoro = saquearValioso unTesoro || saquearObjetoEspecifico unObjeto unTesoro


saquear :: FormaSaqueo -> Pirata -> Tesoro -> [Tesoro]
saquear formaSaqueo unPirata unTesoro = (botin unPirata) ++ filter formaSaqueo [unTesoro]

-- PRUEBAS

-- *Main> saquear (saquearObjetoEspecifico oro) anneBonny oro
-- [("Doblones",10000),("Frasco De Arena",1),("Oro",100)]

-- *Main> saquear saquearConCorazon davidJones oro
-- [("Cajita Musical",1)]

-- *Main> saquear (saquearConjuncion sombrero) jackSparrow oro
-- [("Brujula",10000),("Frasco De Arena",0)]


---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------


--BARCOS

seIncorpora :: Pirata -> Barco -> [Pirata]
seIncorpora unPirata unBarco = unPirata :  (fst unBarco)

seVa :: Pirata -> Barco -> [Pirata]
seVa unPirata unBarco = filter ((/=)unPirata) (fst unBarco)

anclarIslaDesabitada :: Isla -> Barco -> [[Tesoro]]
anclarIslaDesabitada unaIsla unBarco = map ((:) (snd unaIsla)) (map botin (fst unBarco))

atacarUnaCiudad :: Ciudad -> Barco -> FormaSaqueo -> [[Tesoro]]
atacarUnaCiudad unaCiudad unBarco formaSaqueo = zipWith (saquear formaSaqueo) (fst unBarco) (snd unaCiudad)

abordarOtroBarco :: Barco -> Barco -> [[Tesoro]]
abordarOtroBarco unBarco otroBarco
 | (length (fst unBarco)) >= (length (fst otroBarco)) = map (pierdeTodosSusTesorosValiosos) (fst otroBarco)
 | otherwise = map (pierdeTodosSusTesorosValiosos) (fst unBarco)


---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------


--TODA LA PELICULA
{-

*Main> anclarIslaDesabitada islaRon perlaNegra
[[("Botella de Ron",25),("Brujula",10000),("Frasco De Arena",0)],[("Botella de Ron",25),("Doblones",100),("Frasco De Arena",1)]]

*Main> atacarUnaCiudad portRoyal perlaNegra saquearValioso
[[("Brujula",10000),("Frasco De Arena",0),("Brujula",10000)],[("Doblones",100),("Frasco De Arena",1)]]

*Main> anclarIslaDesabitada islaTortuga holandesErrante
[[("Frasco De Arena",1),("Cajita Musical",1)]]

*Main> atacarUnaCiudad carmenPatagones holandesErrante saquearValioso
[[("Cajita Musical",1)]]

*Main> abordarOtroBarco perlaNegra holandesErrante
[[("Cajita Musical",1)]]

-}
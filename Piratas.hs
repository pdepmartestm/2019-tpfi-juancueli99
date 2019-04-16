data Pirata = UnPirata{
    nombre :: String,
    botin :: [Tesoro]}
    deriving (Show,Eq)


type Barco = ([Pirata],FormaSaqueo)

type FormaSaqueo = ([Tesoro] -> [Tesoro])

type Isla = (String,Tesoro)

type Ciudad = (String,[Tesoro])


--PIRATAS

jackSparrow = UnPirata "Jack Sparrow" [brujula,("Frasco De Arena",0),mapa,("Frasco De Arena",1),oro]
davidJones = UnPirata "David Jones" [cajitalMusical]
anneBonny = UnPirata "Anne Bonny" [doblones,("Frasco De Arena",1)]
elizabethSwann = UnPirata "Elizabeth Swann" [monedaCofreMuerto,espadaHierro]
willTurner = UnPirata "Will Turner" [cuchillo]


--TESOROS

type Tesoro = (String, Int)

mapa = ("Mapa",10)
brujula = ("Brujula",10000)
cajitalMusical = ("Cajita Musical",1)
doblones = ("Doblones",10000)
monedaCofreMuerto = ("Moneda Cofre Muerto", 100)
espadaHierro = ("Espada de Hierro", 50)
cuchillo = ("Cuchillo", 5)
botellaRon :: Tesoro
botellaRon = ("Botella de Ron",25)
oro = ("Oro",100)

--BARCOS

perlaNegra = ([jackSparrow,anneBonny],saquearValiosos)
holandesErrante = ([davidJones],saquearConCorazon)

--ISLAS

islaTortuga = ("Isla Tortuga", ("Frasco De Arena",1))
islaRon = ("Isla del Ron", botellaRon)


-- CIUDADES
portRoyal = ("Port Royal", [brujula,doblones,cajitalMusical,monedaCofreMuerto])


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

saquearValiosos :: [Tesoro] -> [Tesoro]
saquearValiosos unTesoro = filter esValioso unTesoro

saquearObjetoEspecifico :: Tesoro -> [Tesoro] -> [Tesoro]
saquearObjetoEspecifico unObjeto unTesoro = filter ((==)unObjeto) unTesoro

saquearConCorazon :: [Tesoro] -> [Tesoro]
saquearConCorazon unTesoro = []

saquearConjuncion :: Tesoro -> [Tesoro] -> [Tesoro]
saquearConjuncion unObjeto unTesoro
  | any esValioso unTesoro  = saquearValiosos unTesoro
  | elem unObjeto unTesoro = saquearObjetoEspecifico unObjeto unTesoro
  | otherwise = saquearConCorazon unTesoro

-- USANDO COMPOSICION?? (saquearConCorazon.saquearValiosos) unTesoro
-- VER QUE PASA SI NO LE PASO EL PARAMETRO unObjeto

saquear :: (t -> [Tesoro]) -> Pirata -> t -> [Tesoro]
saquear formaSaqueo unPirata unTesoro = (formaSaqueo unTesoro) ++ (botin unPirata)

--EJEMPLO (le pasamos el parametro que le falta)
-- *Main> saquear jackSparrow (saquearObjetoEspecifico doblones) [mapa,brujula,doblones]
-- [("Doblones",100),("Brujula",10000),("Frasco De Arena",0),("mapa",10),("Frasco De Arena",1)]



-- PRUEBAS

-- *Main> saquear anneBonny (saquearObjetoEspecifico oro) [oro]
-- [("Oro",100),("Doblones",100),("Frasco De Arena",1)]

-- *Main> saquear davidJones saquearConCorazon [brujula,oro,mapa,doblones]
-- [("Cajita Musical",1)]



---------------------------------------------------------------------------------------------------------------------
---------------------------------------------------------------------------------------------------------------------



--BARCOS

seIncorpora :: Pirata -> Barco -> [Pirata]
seIncorpora unPirata unBarco = unPirata :  (fst unBarco)

seVa :: Pirata -> Barco -> [Pirata]
seVa unPirata unBarco = filter ((/=)unPirata) (fst unBarco)

anclarIslaDesabitada :: Isla -> Barco -> [[Tesoro]]
anclarIslaDesabitada unaIsla unBarco = map ((:) (snd unaIsla)) (map botin (fst unBarco))


haySufiTesoros :: Ciudad -> Barco -> Bool
haySufiTesoros unaCiudad unBarco = length (snd unaCiudad) >= length (fst unBarco)

atacarUnaCiudad :: Ciudad -> Barco -> FormaSaqueo -> [[Tesoro]]
atacarUnaCiudad unaCiudad unBarco formaSaqueo = zipWith (saquear formaSaqueo) (fst unBarco) [(snd unaCiudad)]

abordarOtroBarco :: Barco -> Barco -> [[Tesoro]]
abordarOtroBarco unBarco otroBarco
 | (length (fst unBarco)) >= (length (fst otroBarco)) = map (pierdeTodosSusTesorosValiosos) (fst otroBarco)
 | otherwise = map (pierdeTodosSusTesorosValiosos) (fst unBarco)
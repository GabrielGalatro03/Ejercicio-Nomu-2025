module Library where
import PdePreludat

data Nomu = UnNomu {
    alas :: Bool,
    brazos :: Bool,
    ojos :: Number,
    piel :: String,
    vida :: Number,
    fuerza :: Number
}deriving(Show, Eq)

puedeVer :: Nomu -> Bool
puedeVer nomu = ojos nomu > 0 

categoria :: Nomu -> String
categoria (UnNomu _ _ _ _ _ fuerza)
            | fuerza < 1000 = "pichi"
            | fuerza >= 1000 && fuerza < 3000 = "comun"
            | fuerza >= 3000 && fuerza < 10000 = "fuerte"
            | otherwise = "high-end"

=====================================================================
nomu1 :: Nomu
nomu1 = UnNomu False True 2 "oscuro" 1000 3000

nomu2 :: Nomu
nomu2 = UnNomu True True 1 "blanco" 1000 500

nomu3 :: Nomu
nomu3 = UnNomu True True 3 "blanco" 1000 100

nomu4 :: Nomu
nomu4 = UnNomu True False 2 "oscuro" 1000 6000

nomu5 :: Nomu
nomu5 = UnNomu False True 1 "oscuro" 1000 2500

nomus :: [Nomu]
nomus = [nomu1, nomu2, nomu3, nomu4, nomu5]
========================================================================
entrenaNomu :: Nomu -> Nomu
entrenaNomu nomu = nomu{fuerza = fuerza nomu + 2000}

entrenarEjercito :: [Nomu] -> [Nomu]
entrenarEjercito  = map entrenaNomu 

fuerzaMayorA :: Nomu -> Bool
fuerzaMayorA nomu = fuerza nomu >= 2500

puedenIrAlaGuerra  :: [Nomu] -> Bool
puedenIrAlaGuerra  = all fuerzaMayorA . entrenarEjercito

esFuerte :: Nomu -> Bool
esFuerte nomu = categoria nomu == "fuerte"

nomusFuertes :: [Nomu] -> [Nomu]
nomusFuertes = filter esFuerte 
===========================================================================

esAereo :: Nomu -> Bool
esAereo nomu = alas nomu && not (brazos nomu)

esTerrestre :: Nomu -> Bool
esTerrestre nomu = brazos nomu && not (alas nomu) 

esElegido :: Nomu -> Bool
esElegido nomu = alas nomu && brazos nomu 

cantidadAereos :: [Nomu] -> Number
cantidadAereos = length . filter esAereo

poderTotalTerrestre :: [Nomu] -> Number
poderTotalTerrestre = sum . map fuerza . filter esTerrestre

hayElegido :: [Nomu] -> Bool
hayElegido = any esElegido

entrenamientoElegidos :: [Nomu] -> [Nomu]
entrenamientoElegidos = map entrenaElegido . filter esElegido 

entrenaElegido :: Nomu -> Nomu
entrenaElegido = entrenarNomu . entrenarNomu

module Lib where

import Control.Monad
import Data.List
import Data.Maybe (isJust)

type Colonne = Int
type Ligne = Int
type Score = Int
data Couleur = Jaune | Rouge | Bleu | Vert | Rose | Crane deriving (Show, Eq)
type Grille = [[Couleur]]

hauteur :: Grille -> Colonne -> Ligne
hauteur grille colonne = length (grille !! colonne)

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth n updateFunction (x:xs)
    | n == 0 = (updateFunction x):xs
    | otherwise = x:replaceNth (n-1) updateFunction xs

ajouter :: Colonne -> Couleur -> Grille -> Grille
ajouter colonne couleur grille = replaceNth colonne (const $ reverse (couleur:(reverse $ grille !! colonne))) grille

construireCouleursSuivante :: IO [Couleur]
construireCouleursSuivante = do
    couleursSuivantes <- replicateM 8 getLine
    return (map (\x -> charVersCouleur $ x!!0) couleursSuivantes)

construireGrille :: IO Grille
construireGrille = do
    chainesDeCaracteres <- replicateM 12 getLine
    return (ajouterPlusieursChainesDeCaracteres chainesDeCaracteres [[], [], [], [], [], []])

choisirColonne :: Couleur -> Grille -> Colonne
choisirColonne couleur grille = maximumBy compare' [0..5]
    where
        compare' colonne1 colonne2 = compare (getScore colonne1) (getScore colonne2)
        getScore colonne = (snd $ simplifierGrille (ajouterDeuxBlocs colonne couleur grille)) - length (grille!!colonne)

ajouterPlusieursChainesDeCaracteres :: [String] -> Grille -> Grille
ajouterPlusieursChainesDeCaracteres [] grille = grille
ajouterPlusieursChainesDeCaracteres (x:xs) grille = ajouterPlusieursChainesDeCaracteres xs $ ajouterChaineDeCaractere x grille

ajouterChaineDeCaractere :: String -> Grille -> Grille
ajouterChaineDeCaractere entree grille = zipWith merge entree grille
    where
        merge '.' colonne = colonne
        merge '0' colonne = Crane:colonne
        merge x colonne = (charVersCouleur x):colonne

charVersCouleur :: Char -> Couleur
charVersCouleur '1' = Bleu
charVersCouleur '2' = Jaune
charVersCouleur '3' = Rouge
charVersCouleur '4' = Vert
charVersCouleur '5' = Rose

ajouterDeuxBlocs :: Colonne -> Couleur -> Grille -> Grille
ajouterDeuxBlocs colonne couleur grille = ajouter colonne couleur $ ajouter colonne couleur grille

simplifierGrille :: Grille -> (Grille, Int)
simplifierGrille grille = simplifierGrilleMarquee grilleMarquee 0
    where grilleMarquee = map (map (\x -> (False, x))) grille

simplifierGrilleMarquee :: [[(Bool, Couleur)]] -> Int -> (Grille, Int)
simplifierGrilleMarquee grilleMarquee score = case premierElementNonMarque of
        (Just colonne, Just ligne) -> simplifierGrilleMarquee' $ parcourirGrilleAPartirDe colonne ligne grilleMarquee
        otherwise -> (map (map snd) grilleMarquee, score)
    where
        premierElementNonMarque = (findIndex isJust colonneAvecElementNonMarques, msum colonneAvecElementNonMarques)
        colonneAvecElementNonMarques = map (findIndex (not . fst)) grilleMarquee
        simplifierGrilleMarquee' (nouvelleGrilleMarquee, nouveauScore) = simplifierGrilleMarquee nouvelleGrilleMarquee (nouveauScore + score)

modifierElement :: (a -> a) -> Colonne -> Ligne -> [[a]] -> [[a]]
modifierElement fn colonne ligne grille = replaceNth colonne (const nouvelleColonne) grille
    where nouvelleColonne = replaceNth ligne fn (grille !! colonne)

parcourirGrilleAPartirDe :: Colonne -> Ligne -> [[(Bool, Couleur)]] -> ([[(Bool, Couleur)]], Int)
parcourirGrilleAPartirDe colonne ligne grilleMarquee = if nombreDeCoupFinal < 4 then (virerMaybe grilleMarquee grilleMarqueeFinale, 0) else (virerNothings grilleMarqueeFinale, nombreDeCoupFinal)
    where
        (grilleMarqueeFinale, nombreDeCoupFinal) = parcourir couleurCourante colonne ligne grilleMarqueeMaybe 1
        grilleMarqueeMaybe = supprimerBloc colonne ligne $ map (map (\(marque, couleur) -> (marque, Just couleur))) grilleMarquee
        couleurCourante = snd $ (grilleMarquee !! colonne) !! ligne

blocEstNonMarqueEtMemeCouleur :: Couleur -> Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> [[(Bool, Maybe Couleur)]]
blocEstNonMarqueEtMemeCouleur couleur colonne ligne grille
    | colonne < 0 = grille
    | ligne < 0 = grille
    | colonne >= length grille = grille
    | ligne >= length blocs = grille
    | otherwise = case bloc of
        (False, Just x) -> if (x == couleur || x == Crane) then nouvelleGrilleSucces else grille
        (True, Just x) -> if x == Crane then nouvelleGrilleSucces else grille
        otherwise -> grille
    where
        blocs = grille !! colonne
        bloc = blocs !! ligne
        nouvelleGrilleSucces = supprimerBloc colonne ligne grille

supprimerBloc :: Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> [[(Bool, Maybe Couleur)]]
supprimerBloc = modifierElement (\_ -> (True, Nothing))

parcourir :: Couleur -> Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> Int -> ([[(Bool, Maybe Couleur)]], Int)
parcourir Crane _ _ grilleMarqueeMaybe nombreDeCoup = (grilleMarqueeMaybe, nombreDeCoup)
parcourir couleurCourante colonne ligne grilleMarqueeMaybe nombreDeCoup
    | grilleMarqueeMaybe /= nouvelleGrilleGauche = case parcourirAGauche nouvelleGrilleGauche of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleDroite = case parcourirADroite nouvelleGrilleDroite of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleHaut = case parcourirEnHaut nouvelleGrilleHaut of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleBas = case parcourirEnBas nouvelleGrilleBas of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | otherwise = (grilleMarqueeMaybe, nombreDeCoup)
    where
        blocEstNonMarqueEtMemeCouleur' x y = blocEstNonMarqueEtMemeCouleur couleurCourante x y grilleMarqueeMaybe
        parcourir' x y g = parcourir couleurCourante x y g 1
        nouvelleGrilleGauche = blocEstNonMarqueEtMemeCouleur' (colonne - 1) ligne
        parcourirAGauche = parcourir' (colonne - 1) ligne
        nouvelleGrilleDroite = blocEstNonMarqueEtMemeCouleur' (colonne + 1) ligne
        parcourirADroite = parcourir' (colonne + 1) ligne
        nouvelleGrilleHaut = blocEstNonMarqueEtMemeCouleur' colonne (ligne + 1)
        parcourirEnHaut = parcourir' colonne (ligne + 1)
        nouvelleGrilleBas = blocEstNonMarqueEtMemeCouleur' colonne (ligne - 1)
        parcourirEnBas = parcourir' colonne (ligne + 1)

virerMaybe :: [[(Bool, Couleur)]] -> [[(Bool, Maybe Couleur)]] -> [[(Bool, Couleur)]]
virerMaybe grilleCouleurs grilleMarques = map (map (\((_, couleur), (marque, _)) -> (marque, couleur))) megaZip
    where megaZip = zipWith (zip) grilleCouleurs grilleMarques

virerNothings :: [[(Bool, Maybe Couleur)]] -> [[(Bool, Couleur)]]
virerNothings grilleAvecMaybe = map transform' grilleAvecMaybe

-- | Lift value in the 'Maybe' and abandon 'Nothing'.
transform' :: [(a, Maybe b)] -> [(a, b)]
transform' = map (\(a, Just b) -> (a, b)) . filter (isJust . snd)

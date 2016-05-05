module Lib where

import Control.Monad
import Data.List
import Data.Maybe (isJust)

type Colonne = Int
type Ligne = Int
type Score = Int
data Couleur = Jaune | Rouge | Bleu | Vert | Rose | Crane deriving (Show, Eq)
type Grille = [[Couleur]]
type Paire = (Couleur, Couleur)
type Rotation = Char

complexite :: Grille -> Int
complexite grille = min 3 (50 `div` nombreDeBlocs)
    where nombreDeBlocs = max 1 (sum (map length grille))

hauteur :: Grille -> Colonne -> Ligne
hauteur grille colonne
    | colonne < 0 = 12
    | colonne > 5 = 12
    | otherwise = length (grille !! colonne)

replaceNth :: Int -> (a -> a) -> [a] -> [a]
replaceNth n updateFunction (x:xs)
    | n == 0 = updateFunction x:xs
    | otherwise = x:replaceNth (n-1) updateFunction xs

choix :: [(Colonne, Rotation)]
choix = [(colonne, rotation) | colonne <- [0..5], rotation <- ['0'..'3']]

ajouter :: Colonne -> Couleur -> Grille -> Maybe Grille
ajouter colonne couleur grille
    | colonne < 0 = Nothing
    | colonne > 5 = Nothing
    | otherwise = Just $ replaceNth colonne (const $ reverse (couleur : reverse (grille !! colonne))) grille

construireCouleursSuivante :: IO [Paire]
construireCouleursSuivante = do
    couleursSuivantes <- replicateM 8 getLine
    return (map (\x -> (charVersCouleur $ head x, charVersCouleur $ x!!2)) couleursSuivantes)

construireGrille :: IO Grille
construireGrille = do
    chainesDeCaracteres <- replicateM 12 getLine
    return (ajouterPlusieursChainesDeCaracteres (reverse chainesDeCaracteres) [[], [], [], [], [], []])

getScoreSurPlusieursTours :: Colonne -> Rotation -> Paire -> [Paire] -> Grille -> Int
getScoreSurPlusieursTours colonne rotation paire [] grille = snd $ getScore colonne paire rotation grille
getScoreSurPlusieursTours colonne rotation paire (paireSuivante:pairesSuivantes) grille = scoreActuel + maxScoreSuivant
    where
        (grilleSuivante, scoreActuel) = getScore colonne paire rotation grille
        maxScoreSuivant = maximum (map getScoreSurPlusieursTours' choix)
        getScoreSurPlusieursTours' (colonne, rotation) = getScoreSurPlusieursTours colonne rotation paireSuivante pairesSuivantes grilleSuivante

getScore :: Colonne -> Paire -> Rotation -> Grille -> (Grille, Int)
getScore colonne paire rotation grille
    | score < (-100) = (grille, -1000)
    | otherwise = (grilleSuivante, 2 ^ score - 2 ^ hauteur grilleSuivante colonne - hauteur grilleSuivante (colonne - 1) - hauteur grilleSuivante (colonne + 1))
    where
        (grilleSuivante, score) = case ajouterPaire colonne rotation paire grille of
            Just grille' -> simplifierGrille grille'
            Nothing -> (grille, -1000)

choisir :: Paire -> Grille -> (Colonne, Rotation)
choisir paire grille = maximumBy compare' choix
    where
        compare' choix1 choix2 = compare (getScore' choix1) (getScore' choix2)
        getScore' (colonne, rotation) = snd $ getScore colonne paire rotation grille

choisirSurPlusieursTours :: [Paire] -> Grille -> (Colonne, Rotation)
choisirSurPlusieursTours (paire:pairesSuivantes) grille = maximumBy compare' choix
    where
        compare' choix1 choix2 = compare (getScore' choix1) (getScore' choix2)
        getScore' (colonne, rotation) = getScoreSurPlusieursTours colonne rotation paire pairesSuivantes grille

ajouterPlusieursChainesDeCaracteres :: [String] -> Grille -> Grille
ajouterPlusieursChainesDeCaracteres xs grille = foldl (flip ajouterChaineDeCaractere) grille xs

ajouterChaineDeCaractere :: String -> Grille -> Grille
ajouterChaineDeCaractere = zipWith merge
    where
        merge '.' colonne = colonne
        merge '0' colonne = Crane:colonne
        merge x colonne = charVersCouleur x : colonne

charVersCouleur :: Char -> Couleur
charVersCouleur '1' = Bleu
charVersCouleur '2' = Jaune
charVersCouleur '3' = Rouge
charVersCouleur '4' = Vert
charVersCouleur '5' = Rose
charVersCouleur x = error (x:" pas connu")

ajouterPaire :: Colonne -> Rotation -> Paire -> Grille -> Maybe Grille
ajouterPaire colonne '0' (couleurGauche, couleurDroite) grille = ajouter (colonne + 1) couleurDroite grille >>= ajouter colonne couleurGauche
ajouterPaire colonne '1' (couleurGauche, couleurDroite) grille = ajouter colonne couleurGauche grille >>= ajouter colonne couleurDroite
ajouterPaire colonne '2' (couleurGauche, couleurDroite) grille = ajouter colonne couleurDroite grille >>= ajouter colonne couleurGauche
ajouterPaire colonne '3' (couleurGauche, couleurDroite) grille = ajouter colonne couleurDroite grille >>= ajouter colonne couleurGauche

ajouterDeuxBlocs :: Colonne -> Couleur -> Grille -> Maybe Grille
ajouterDeuxBlocs colonne couleur grille = ajouter colonne couleur grille >>= ajouter colonne couleur

simplifierGrille :: Grille -> (Grille, Int)
simplifierGrille grille = simplifierGrilleMarquee grilleMarquee 0
    where grilleMarquee = map (map (\x -> (False, x))) grille

simplifierGrilleMarquee :: [[(Bool, Couleur)]] -> Int -> (Grille, Int)
simplifierGrilleMarquee grilleMarquee score = case premierElementNonMarque of
        (Just colonne, Just ligne) -> simplifierGrilleMarquee' $ parcourirGrilleAPartirDe colonne ligne grilleMarquee
        _ -> (map (map snd) grilleMarquee, score)
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

blocEstNonMarqueEtMemeCouleur :: Couleur -> Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> (Bool, [[(Bool, Maybe Couleur)]])
blocEstNonMarqueEtMemeCouleur couleur colonne ligne grille
    | colonne < 0 = (False, grille)
    | ligne < 0 = (False, grille)
    | colonne >= length grille = (False, grille)
    | ligne >= length blocs = (False, grille)
    | otherwise = case bloc of
        (_, Just Crane) -> (True, nouvelleGrilleSucces)
        (False, Just x) -> if x == couleur then (False, nouvelleGrilleSucces) else (False, grille)
        _ -> (False, grille)
    where
        blocs = grille !! colonne
        bloc = blocs !! ligne
        nouvelleGrilleSucces = supprimerBloc colonne ligne grille

supprimerBloc :: Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> [[(Bool, Maybe Couleur)]]
supprimerBloc = modifierElement (const (True, Nothing))

parcourir :: Couleur -> Colonne -> Ligne -> [[(Bool, Maybe Couleur)]] -> Int -> ([[(Bool, Maybe Couleur)]], Int)
parcourir Crane _ _ grilleMarqueeMaybe nombreDeCoup = (grilleMarqueeMaybe, nombreDeCoup)
parcourir couleurCourante colonne ligne grilleMarqueeMaybe nombreDeCoup
    | grilleMarqueeMaybe /= nouvelleGrilleGauche = case (if craneAGauche then (nouvelleGrilleGauche, 0) else parcourirAGauche nouvelleGrilleGauche) of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleDroite = case (if craneADroite then (nouvelleGrilleDroite, 0) else parcourirADroite nouvelleGrilleDroite) of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleHaut = case (if craneEnHaut then (nouvelleGrilleHaut, 0) else parcourirEnHaut nouvelleGrilleHaut) of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | grilleMarqueeMaybe /= nouvelleGrilleBas = case (if craneEnBas then (nouvelleGrilleBas, 0) else parcourirEnBas nouvelleGrilleBas) of
        (nouvelleGrille, nombreDeCoupAjoute) -> parcourir couleurCourante colonne ligne nouvelleGrille (nombreDeCoup + nombreDeCoupAjoute)
    | otherwise = (grilleMarqueeMaybe, nombreDeCoup)
    where
        blocEstNonMarqueEtMemeCouleur' x y = blocEstNonMarqueEtMemeCouleur couleurCourante x y grilleMarqueeMaybe
        parcourir' x y g = parcourir couleurCourante x y g 1
        (craneAGauche, nouvelleGrilleGauche) = blocEstNonMarqueEtMemeCouleur' (colonne - 1) ligne
        parcourirAGauche = parcourir' (colonne - 1) ligne
        (craneADroite, nouvelleGrilleDroite) = blocEstNonMarqueEtMemeCouleur' (colonne + 1) ligne
        parcourirADroite = parcourir' (colonne + 1) ligne
        (craneEnHaut, nouvelleGrilleHaut) = blocEstNonMarqueEtMemeCouleur' colonne (ligne + 1)
        parcourirEnHaut = parcourir' colonne (ligne + 1)
        (craneEnBas, nouvelleGrilleBas) = blocEstNonMarqueEtMemeCouleur' colonne (ligne - 1)
        parcourirEnBas = parcourir' colonne (ligne + 1)

virerMaybe :: [[(Bool, Couleur)]] -> [[(Bool, Maybe Couleur)]] -> [[(Bool, Couleur)]]
virerMaybe grilleCouleurs grilleMarques = map (map (\((_, couleur), (marque, _)) -> (marque, couleur))) megaZip
    where megaZip = zipWith zip grilleCouleurs grilleMarques

virerNothings :: [[(Bool, Maybe Couleur)]] -> [[(Bool, Couleur)]]
virerNothings = map transform'

-- | Lift value in the 'Maybe' and abandon 'Nothing'.
transform' :: [(a, Maybe b)] -> [(a, b)]
transform' = map (\(a, Just b) -> (a, b)) . filter (isJust . snd)

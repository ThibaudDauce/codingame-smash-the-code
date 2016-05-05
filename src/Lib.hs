module Lib where

import Control.Monad
import Data.List (findIndex, maximumBy)
import Data.Maybe (isJust, isNothing, fromJust)
import Data.Matrix
import qualified Data.Vector as V

type Colonne = Int
type Ligne = Int
type Score = Int
data Couleur = Jaune | Rouge | Bleu | Vert | Rose | Crane deriving (Show, Eq)
type Grille = Matrix (Maybe Couleur)
type Paire = (Couleur, Couleur)
type Rotation = Char

hauteur :: Grille -> Colonne -> Ligne
hauteur grille colonne
    | colonne < 1 = 12
    | colonne > 6 = 12
    | otherwise = V.length (V.filter isJust $ getCol colonne grille)

choix :: [(Colonne, Rotation)]
choix = [(colonne, rotation) | colonne <- [1..6], rotation <- ['0'..'3']]

ajouter :: Colonne -> Couleur -> Grille -> Maybe Grille
ajouter colonne couleur grille
    | colonne < 1 = Nothing
    | colonne > 6 = Nothing
    | hauteurColonne > 11 = Nothing
    | otherwise = Just $ setElem (Just couleur) (12 - hauteurColonne, colonne) grille
    where hauteurColonne = hauteur grille colonne

construireCouleursSuivante :: IO [Paire]
construireCouleursSuivante = do
    couleursSuivantes <- replicateM 8 getLine
    return (map (\x -> (charVersCouleur $ head x, charVersCouleur $ x!!2)) couleursSuivantes)

construireGrille :: IO Grille
construireGrille = do
    chainesDeCaracteres <- replicateM 12 getLine
    return $ fromLists $ map (map charVersBloc) chainesDeCaracteres

charVersBloc :: Char -> Maybe Couleur
charVersBloc '.' = Nothing
charVersBloc '0' = Just Crane
charVersBloc x = Just $ charVersCouleur x

getScoreSurPlusieursTours :: Colonne -> Rotation -> Paire -> [Paire] -> Grille -> Maybe Int
getScoreSurPlusieursTours colonne rotation paire [] grille = snd $ getScore colonne paire rotation grille
getScoreSurPlusieursTours colonne rotation paire (paireSuivante:pairesSuivantes) grille = liftM2 (+) scoreActuel maxScoreSuivant
    where
        (grilleSuivante, scoreActuel) = getScore colonne paire rotation grille
        maxScoreSuivant = maximum (map getScoreSurPlusieursTours' choix)
        getScoreSurPlusieursTours' (colonne, rotation) = getScoreSurPlusieursTours colonne rotation paireSuivante pairesSuivantes grilleSuivante

getScore :: Colonne -> Paire -> Rotation -> Grille -> (Grille, Maybe Int)
getScore colonne paire rotation grille
    | score == (minBound) = (grille, Nothing)
    | otherwise = (grilleSuivante, Just $ score - (maximum $ map (hauteur grilleSuivante) [1..6]))
    where
        (grilleSuivante, score) = case ajouterPaire colonne rotation paire grille of
            Just grille' -> simplifierGrille grille'
            Nothing -> (grille, minBound)

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
ajouterPaire colonne '2' (couleurGauche, couleurDroite) grille = ajouter (colonne - 1) couleurDroite grille >>= ajouter colonne couleurGauche
ajouterPaire colonne '3' (couleurGauche, couleurDroite) grille = ajouter colonne couleurDroite grille >>= ajouter colonne couleurGauche

getVisitedMatrix :: Grille -> Matrix Bool
getVisitedMatrix grille = matrix (nrows grille) (ncols grille) generatorFunction
    where generatorFunction (row, col) = isNothing $ getElem row col grille

isVisited :: Colonne -> Ligne -> Matrix Bool -> Bool
isVisited row col isVisitedMatrix = case safeGet row col isVisitedMatrix of
    Just x -> x
    Nothing -> True

simplifierGrille :: Grille -> (Grille, Int)
simplifierGrille grille = simplifierGrilleVisited grille (getVisitedMatrix grille) 1 1 0

simplifierGrilleVisited :: Grille -> Matrix Bool -> Colonne -> Ligne -> Int -> (Grille, Int)
simplifierGrilleVisited grille isVisitedMatrix 7 12 score = (grille, score)
simplifierGrilleVisited grille isVisitedMatrix 7 row score =
    simplifierGrilleVisited grille isVisitedMatrix 1 (row + 1) score
simplifierGrilleVisited grille isVisitedMatrix col row score = case getElem row col isVisitedMatrix of
    True -> simplifierGrilleVisited grille isVisitedMatrix (col + 1) row score
    False -> simplifierGrilleVisited' $ rechercherMemeCouleurAPartirDe col row grille isVisitedMatrix
    where
        simplifierGrilleVisited' (newGrille, newIsVisitedMatrix, nouveauScore, modifiee)
            | not modifiee = simplifierGrilleVisited grille newIsVisitedMatrix (col + 1) row (score + nouveauScore)
            | otherwise = simplifierGrilleVisited newGrille (getVisitedMatrix newGrille) 1 1 (score + nouveauScore)

rechercherMemeCouleurAPartirDe :: Colonne -> Ligne -> Grille -> Matrix Bool -> (Grille, Matrix Bool, Int, Bool)
rechercherMemeCouleurAPartirDe col row grille isVisitedMatrix
    | nombreDeCoup > 3 = (reduire newGrille, newIsVisitedMatrix, 4 ^ nombreDeCoup, True)
    | otherwise = (grille, newIsVisitedMatrix, 3 ^ nombreDeCoup, False)
    where
        couleur = fromJust $ getElem row col grille
        (newGrille, newIsVisitedMatrix, nombreDeCoup) = rechercherMemeCouleur col row grille isVisitedMatrix couleur 0

rechercherMemeCouleur :: Colonne -> Ligne -> Grille -> Matrix Bool -> Couleur -> Int -> (Grille, Matrix Bool, Int)
rechercherMemeCouleur col row grille isVisitedMatrix couleur nombreDeCoup
    | not visited = seRappeler (remove col row) (setIsVisited col row) 1
    | not visitedAGauche && isCorrect blocAGauche = selfCall (col - 1) row
    | isCrane blocAGauche = seRappeler (remove (col - 1) row) isVisitedMatrix 0
    | not visitedADroite && isCorrect blocADroite = selfCall (col + 1) row
    | isCrane blocADroite = seRappeler (remove (col + 1) row) isVisitedMatrix 0
    | not visitedEnBas && isCorrect blocEnBas = selfCall col (row + 1)
    | isCrane blocEnBas = seRappeler (remove col (row + 1)) isVisitedMatrix 0
    | not visitedEnHaut && isCorrect blocEnHaut = selfCall col (row - 1)
    | isCrane blocEnHaut = seRappeler (remove col (row - 1)) isVisitedMatrix 0
    | otherwise = (grille, isVisitedMatrix, nombreDeCoup)
    where
        isCorrect x = x == Just (Just couleur)
        isCrane x = x == Just (Just Crane)
        bloc = safeGet row col grille
        visited = isVisited row col isVisitedMatrix
        blocAGauche = safeGet row (col - 1) grille
        visitedAGauche = isVisited row (col - 1) isVisitedMatrix
        blocADroite = safeGet row (col + 1) grille
        visitedADroite = isVisited row (col + 1) isVisitedMatrix
        blocEnBas = safeGet (row + 1) col grille
        visitedEnBas = isVisited (row + 1) col isVisitedMatrix
        blocEnHaut = safeGet (row - 1) col grille
        visitedEnHaut = isVisited (row - 1) col isVisitedMatrix
        setIsVisited col' row' = setElem True (row', col') isVisitedMatrix
        remove col' row' = setElem Nothing (row', col') grille
        seRappeler newGrille newIsVisitedMatrix addNombreDeCoup = rechercherMemeCouleur col row newGrille newIsVisitedMatrix couleur (nombreDeCoup + addNombreDeCoup)
        selfCall col' row' = case rechercherMemeCouleur col' row' grille isVisitedMatrix couleur 0 of
            (newGrille, newIsVisitedMatrix, newNombreDeCoup) -> seRappeler newGrille newIsVisitedMatrix newNombreDeCoup

reduire :: Grille -> Grille
reduire grille = matrix 12 6 generator
    where
        generator (row, col) = elementNonNul (12 - row) (getCol col grille)
        elementNonNul numero vecteur = (V.reverse (V.map (\(Just x) -> x) $ V.filter isJust vecteur)) V.!? numero

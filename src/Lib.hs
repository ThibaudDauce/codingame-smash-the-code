module Lib where

import qualified Data.Map as M

type Colonne = Int
type Ligne = Int
type Score = Int
data Couleur = Jaune | Rouge | Bleu | Vert | Rose deriving (Show, Eq)
type Grille = M.Map (Colonne, Ligne) Couleur

premiereLigneVide :: Grille -> Colonne -> Ligne
premiereLigneVide grille colonne = testLigne grille colonne 1
    where testLigne grille colonne ligne = case M.lookup (colonne, ligne) grille of
            Just couleur -> testLigne grille colonne (ligne + 1)
            Nothing -> ligne

ajouter :: Grille -> Colonne -> Couleur -> Grille
ajouter grille colonne couleur = M.insert (colonne, premiereLigneVide grille colonne) couleur grille

ajouterDeuxBlocs :: Grille -> Colonne -> Couleur -> Grille
ajouterDeuxBlocs grille colonne couleur = M.insert (colonne, premiereLigneVide') couleur $ M.insert (colonne, premiereLigneVide' + 1) couleur grille
    where premiereLigneVide' = premiereLigneVide grille colonne

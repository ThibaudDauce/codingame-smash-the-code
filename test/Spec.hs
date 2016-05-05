import Test.Hspec

import qualified Data.Matrix as M
import Data.Maybe (fromJust)
import Lib

main :: IO ()
main = hspec $ do
    describe "Hauteur de la grille" $ do
        it "Pour une grille vide, la hauteur de la colonne 0 est 0" $ do
            (hauteur emptyGrille 1) `shouldBe` 0
        it "Pour une colonne avec un élément, la hauteur est 1" $ do
            (hauteur (fromIncompleteLists [[Bleu], [], [], [], [], []]) 1) `shouldBe` 1
        it "Pour une colonne 1 avec un élément, la hauteur de la colonne 1 est 0" $ do
            (hauteur (fromIncompleteLists [[Bleu], [], [], [], [], []]) 2) `shouldBe` 0
    describe "Ajout de bloc" $ do
        it "Si on ajoute un bloc, il doit être présent" $ do
            (ajouter 6 Bleu emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [], [Bleu]]))
        it "Si on ajoute deux blocs, ils doivent être présents" $ do
            (ajouterPaire 6 '1' (Bleu, Bleu) emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [], [Bleu, Bleu]]))
        it "Si on ajoute un bloc sur un existant, il doit être au dessus" $ do
            (ajouter 6 Vert (fromIncompleteLists [[], [], [], [], [], [Bleu]])) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [], [Bleu, Vert]]))
        it "Si on ajoute un bloc sur une colonne pleine, ça ne doit pas planter" $ do
            (ajouter 6 Vert (fromIncompleteLists [[], [], [], [], [], (replicate 12 Bleu)])) `shouldBe` Nothing
    describe "Ajouter paire" $ do
        it "Retourne nothing si on sort à droite" $ do
            (ajouterPaire 6 '0' (Vert, Vert) emptyGrille) `shouldBe` Nothing
        it "Positionne bien la rotation droite" $ do
            (ajouterPaire 5 '0' (Vert, Jaune) emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [Vert], [Jaune]]))
        it "Ça tombe bien où il faut avec la rotation droite" $ do
            (ajouterPaire 5 '0' (Vert, Jaune) (fromIncompleteLists [[], [], [], [], [], [Bleu]])) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [Vert], [Bleu, Jaune]]))
        it "Positionne bien la rotation gauche" $ do
            (ajouterPaire 5 '2' (Vert, Jaune) emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [Jaune], [Vert], []]))
        it "Ça tombe bien où il faut avec la rotation gauche" $ do
            (ajouterPaire 5 '2' (Vert, Jaune) (fromIncompleteLists [[], [], [], [], [Bleu], []])) `shouldBe` (Just (fromIncompleteLists [[], [], [], [Jaune], [Bleu, Vert], []]))
        it "Positionne bien la rotation droite" $ do
            (ajouterPaire 5 '1' (Vert, Jaune) emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [Vert, Jaune], []]))
        it "Positionne bien la rotation inversé droite" $ do
            (ajouterPaire 5 '3' (Vert, Jaune) emptyGrille) `shouldBe` (Just (fromIncompleteLists [[], [], [], [], [Jaune, Vert], []]))
    describe "Réduire une grille" $ do
        it "Le bloc tombe bien" $ do
            (reduire (M.setElem (Just Bleu) (2, 3) emptyGrille)) `shouldBe` (fromIncompleteLists [[], [], [Bleu], [], [], []])
        it "Deux blocs tombent bien" $ do
            (reduire (M.setElem (Just Vert) (1, 3) $ M.setElem (Just Bleu) (2, 3) emptyGrille)) `shouldBe` (fromIncompleteLists [[], [], [Bleu, Vert], [], [], []])
        it "Deux blocs espacés tombent bien" $ do
            (reduire (M.setElem (Just Vert) (1, 3) $ M.setElem (Just Bleu) (5, 3) emptyGrille)) `shouldBe` (fromIncompleteLists [[], [], [Bleu, Vert], [], [], []])
        it "Une colonne déjà bonne ne bouge pas" $ do
            (reduire (fromIncompleteLists [[], [], [Bleu, Vert], [], [], []])) `shouldBe` (fromIncompleteLists [[], [], [Bleu, Vert], [], [], []])
        it "Une colonne remplie ne bouge pas" $ do
            (reduire (fromIncompleteLists [[], [], [Jaune, Bleu, Vert, Rouge, Rose, Crane, Jaune, Bleu, Vert, Rouge, Rose, Crane], [], [], []])) `shouldBe` (fromIncompleteLists [[], [], [Jaune, Bleu, Vert, Rouge, Rose, Crane, Jaune, Bleu, Vert, Rouge, Rose, Crane], [], [], []])
    describe "Rechercher même couleur" $ do
        it "Remplace un élément vert par Nothing et le marque comme vu" $ do
            (rechercherMemeCouleur 1 10 grilleComplexe (getVisitedMatrix grilleComplexe) Vert 0) `shouldBe` (M.setElem Nothing (10, 1) grilleComplexe, M.setElem True (10, 1) (getVisitedMatrix grilleComplexe), 1)
        it "Remplace les trois rouges par Nothing et les marque comme vus" $ do
            (rechercherMemeCouleur 3 11 grilleComplexe (getVisitedMatrix grilleComplexe) Rouge 0) `shouldBe` (M.setElem Nothing (11, 3) $ M.setElem Nothing (11, 2) $ M.setElem Nothing (11, 1) grilleComplexe, M.setElem True (11, 3) $ M.setElem True (11, 2) $ M.setElem True (11, 1) (getVisitedMatrix grilleComplexe), 3)
        it "Remplace les sept bleus par Nothing et les marque comme vus" $ do
            (rechercherMemeCouleur 3 12 grilleComplexe (getVisitedMatrix grilleComplexe) Bleu 0) `shouldBe` (M.setElem Nothing (10, 3) $ M.setElem Nothing (10, 4) $ M.setElem Nothing (11, 4) $ M.setElem Nothing (12, 4) $ M.setElem Nothing (12, 3) $ M.setElem Nothing (12, 2) $ M.setElem Nothing (12, 1) grilleComplexe, M.setElem True (10, 3) $ M.setElem True (10, 4) $ M.setElem True (11, 4) $ M.setElem True (12, 4) $ M.setElem True (12, 3) $ M.setElem True (12, 2) $ M.setElem True (12, 1) (getVisitedMatrix grilleComplexe), 7)
    describe "Rechercher même couleur à partir de…" $ do
        it "Marque l'élément vert comme vu mais ne le supprime pas" $ do
            (rechercherMemeCouleurAPartirDe 1 10 grilleComplexe (getVisitedMatrix grilleComplexe)) `shouldBe` (grilleComplexe, M.setElem True (10, 1) (getVisitedMatrix grilleComplexe), 2 ^ 1, False)
        it "Marque les trois rouges comme vus mais ne les supprime pas" $ do
            (rechercherMemeCouleurAPartirDe 3 11 grilleComplexe (getVisitedMatrix grilleComplexe)) `shouldBe` (grilleComplexe, M.setElem True (11, 3) $ M.setElem True (11, 2) $ M.setElem True (11, 1) (getVisitedMatrix grilleComplexe), 2 ^ 3, False)
        it "Marque les sept bleus comme vus et les supprime de la grille" $ do
            (rechercherMemeCouleurAPartirDe 3 12 grilleComplexe (getVisitedMatrix grilleComplexe)) `shouldBe` (grilleComplexeSansBleu, M.setElem True (10, 3) $ M.setElem True (10, 4) $ M.setElem True (11, 4) $ M.setElem True (12, 4) $ M.setElem True (12, 3) $ M.setElem True (12, 2) $ M.setElem True (12, 1) (getVisitedMatrix grilleComplexe), 4 ^ 7, True)
    describe "Simplifier grille visitée" $ do
        it "Supprime les rouges et les bleus de la grille" $ do
            (simplifierGrilleVisited grilleComplexe (getVisitedMatrix grilleComplexe) 1 1 0) `shouldBe` (grilleComplexeSimplifie, 16648)
        it "Supprime les verts et les cranes de la grille" $ do
            (simplifierGrilleVisited (fromJust $ ajouterPaire 6 '2' (Vert, Vert) grille4) (getVisitedMatrix (fromJust $ ajouterPaire 6 '2' (Vert, Vert) grille4)) 1 1 0) `shouldBe` (grille4Simplifiee, 340)
    describe "Récupérer le score" $ do
        it "Score exemple 4 bon" $ do
            (snd $ getScore 6 (Vert, Vert) '3' grille4) `shouldBe` Just 331
        it "Score exemple 4 mauvais" $ do
            (snd $ getScore 5 (Vert, Vert) '2' grille4) `shouldBe` Just 87
        it "Score exemple 5 bon" $ do
            (snd $ getScore 3 (Jaune, Rose) '0' grille5) `shouldBe` Just 87
        it "Score exemple 5 mauvais" $ do
            (snd $ getScore 3 (Jaune, Rose) '2' grille5) `shouldBe` Just 87
    describe "Choisir colonne" $ do
        it "Grille example 1" $ do
            (choisir (Rose, Rose) grille1) `shouldBe` (6, '2')
        it "Grille example 2" $ do
            (choisirSurPlusieursTours [(Bleu, Bleu), (Bleu, Bleu)] grille2) `shouldBe` (3, '3')
        it "Grille example 3" $ do
            (choisir (Bleu, Bleu) grille3) `shouldBe` (3, '2')
        it "Grille example 4" $ do
            (choisir (Vert, Vert) grille4) `shouldBe` (6, '3')
        it "Grille example 5" $ do
            (choisir (Jaune, Rose) grille5) `shouldBe` (3, '0')
        it "Grille vide" $ do
            (choisirSurPlusieursTours [(Vert, Vert), (Jaune, Vert)] emptyGrille) `shouldBe` (6, '2')
--
grilleComplexe :: Grille
grilleComplexe = fromIncompleteLists $ [[Bleu, Rouge, Vert], [Bleu, Rouge], [Bleu, Rouge, Bleu, Rouge], [Bleu, Bleu, Bleu], [], []]

grilleComplexeSansBleu :: Grille
grilleComplexeSansBleu = fromIncompleteLists $ [[Rouge, Vert], [Rouge], [Rouge, Rouge], [], [], []]

grilleComplexeSimplifie :: Grille
grilleComplexeSimplifie = fromIncompleteLists $ [[Vert], [], [], [], [], []]

emptyGrille :: Grille
emptyGrille = fromIncompleteLists $ [[], [], [], [], [], []]

grille1 :: Grille
grille1 = fromIncompleteLists $ [[], [], [Jaune, Jaune], [Rouge, Rouge], [Rose, Rose], [Bleu, Bleu]]

mauvaiseGrille1 :: Grille
mauvaiseGrille1 = fromIncompleteLists $ [[], [], [Jaune, Jaune], [Rouge, Rouge], [Rose, Rose], [Bleu, Bleu, Rose, Rose]]

grilleCrane :: Grille
grilleCrane = fromIncompleteLists $ [[Crane], [], [Crane, Crane], [Rouge], [Rouge, Rouge, Rouge], []]

grilleCraneSimplifiee :: Grille
grilleCraneSimplifiee = fromIncompleteLists $ [[Crane], [], [Crane], [], [], []]

grille2 :: Grille
grille2 = fromIncompleteLists $ [[], [Bleu, Bleu], [Jaune, Jaune], [], [], []]

grille3 :: Grille
grille3 = fromIncompleteLists $ [[Bleu], [], [], [Bleu], [], []]

fromIncompleteLists :: [[Couleur]] -> Grille
fromIncompleteLists lists = M.transpose $ M.fromLists $ map fillList lists
    where fillList incompleteColonne = lpad 12 $ reverse $ map Just incompleteColonne

lpad m xs = replicate (m - length ys) Nothing ++ ys
    where ys = take m xs

grille4 :: Grille
grille4 = fromIncompleteLists $ [[Bleu, Vert, Crane, Rose, Bleu, Vert, Crane, Jaune, Rouge], [Crane, Rouge, Bleu, Vert, Crane, Rose, Bleu], [Vert, Rouge, Crane, Jaune, Rouge, Crane, Jaune], [Rouge, Rose, Crane, Bleu, Rose, Crane, Bleu], [Bleu, Rose, Jaune, Crane, Vert, Rose, Crane, Vert], [Vert, Jaune, Crane, Rouge, Bleu, Crane, Vert]]

grille4Simplifiee :: Grille
grille4Simplifiee = fromIncompleteLists $ [[Bleu, Vert, Crane, Rose, Bleu, Vert, Crane, Jaune, Rouge], [Crane, Rouge, Bleu, Vert, Crane, Rose, Bleu], [Vert, Rouge, Crane, Jaune, Rouge, Crane, Jaune], [Rouge, Rose, Crane, Bleu, Rose, Crane, Bleu], [Bleu, Rose, Jaune, Crane, Vert, Rose], [Vert, Jaune, Crane, Rouge, Bleu]]

grille5 :: Grille
grille5 = fromIncompleteLists $ [[], [Bleu, Bleu], [Jaune, Jaune], [Rouge, Rose], [Vert, Vert], [Vert, Bleu]]

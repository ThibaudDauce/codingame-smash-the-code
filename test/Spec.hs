import Test.Hspec

import qualified Data.Map as M
import Lib

main :: IO ()
main = hspec $ do
    describe "Hauteur de la grille" $ do
        it "Pour une grille vide, la hauteur de la colonne 0 est 0" $ do
            (hauteur [[], [], [], [], [], []] 0) `shouldBe` 0
        it "Pour une colonne avec un élément, la hauteur est 1" $ do
            (hauteur [[Bleu], [], [], [], [], []] 0) `shouldBe` 1
        it "Pour une colonne 1 avec un élément, la hauteur de la colonne 1 est 0" $ do
            (hauteur [[Bleu], [], [], [], [], []] 1) `shouldBe` 0
    describe "Ajout de bloc" $ do
        it "Si on ajoute un bloc, la hauteur doit être 1" $ do
            (hauteur (ajouter 0 Bleu [[], [], [], [], [], []]) 0) `shouldBe` 1
        it "Si on ajoute deux blocs, la hauteur doit être 2" $ do
            (hauteur (ajouterDeuxBlocs 0 Bleu [[], [], [], [], [], []]) 0) `shouldBe` 2
        it "Si on ajoute un bloc, il doit être présent" $ do
            (ajouter 5 Bleu [[], [], [], [], [], []]) `shouldBe` [[], [], [], [], [], [Bleu]]
        it "Si on ajoute deux blocs, ils doivent être présents" $ do
            (ajouterDeuxBlocs 5 Bleu [[], [], [], [], [], []]) `shouldBe` [[], [], [], [], [], [Bleu, Bleu]]
        it "Si on ajoute un bloc sur un existant, il doit être au dessus" $ do
            (ajouter 5 Vert [[], [], [], [], [], [Bleu]]) `shouldBe` [[], [], [], [], [], [Bleu, Vert]]
    describe "Bloc marque de la même couleur" $ do
        it "Ne change pas la grille si l'on demande une colonne inférieure à 0" $ do
            (blocEstNonMarqueEtMemeCouleur Bleu (-1) 2 grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Ne change pas la grille si l'on demande une ligne inférieure à 0" $ do
            (blocEstNonMarqueEtMemeCouleur Bleu 2 (-1) grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Ne change pas la grille si l'on demande une colonne supérieure à 6" $ do
            (blocEstNonMarqueEtMemeCouleur Bleu 6 2 grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Ne change pas la grille si l'on demande une ligne supérieure à sa hauteur" $ do
            (blocEstNonMarqueEtMemeCouleur Bleu 0 3 grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Ne change pas la grille si l'on demande une ligne vide" $ do
            (blocEstNonMarqueEtMemeCouleur Bleu 5 0 grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Ne change rien si la marque est déjà posée et que la couleur correspond" $ do
            (blocEstNonMarqueEtMemeCouleur Vert 0 0 (modifierElement (const (True, Just Vert)) 0 0 grilleComplexeMaybe)) `shouldBe` (modifierElement (const (True, Just Vert)) 0 0 grilleComplexeMaybe)
        it "Ne change rien si la marque est déjà posée et que la couleur ne correspond pas" $ do
            (blocEstNonMarqueEtMemeCouleur Vert 0 0 (modifierElement (const (True, Just Bleu)) 0 0 grilleComplexeMaybe)) `shouldBe` (modifierElement (const (True, Just Bleu)) 0 0 grilleComplexeMaybe)
        it "Ne change rien si la couleur n'est pas bonne" $ do
            (blocEstNonMarqueEtMemeCouleur Vert 0 0 grilleComplexeMaybe) `shouldBe` grilleComplexeMaybe
        it "Change la marque et le maybe si la couleur est bonne" $ do
            (blocEstNonMarqueEtMemeCouleur Vert 0 2 grilleComplexeMaybe) `shouldBe` (modifierElement (const (True, Nothing)) 0 2 grilleComplexeMaybe)
    describe "Parcourir la grille" $ do
        it "Marque le vert seulement" $ do
            (parcourir Vert 0 2 (supprimerBloc 0 2 grilleComplexeMaybe) 10) `shouldBe` (supprimerBloc 0 2 grilleComplexeMaybe, 10)
        it "Marque le vert seulement si le bloc d'en dessous est supprimé" $ do
            (parcourir Vert 0 2 (supprimerBloc 0 1 $ supprimerBloc 0 2 grilleComplexeMaybe) 10) `shouldBe` (supprimerBloc 0 1 $ supprimerBloc 0 2 grilleComplexeMaybe, 10)
        it "Marque et supprime les trois rouges depuis la gauche" $ do
            (parcourir Rouge 0 1 (supprimerBloc 0 1 grilleComplexeMaybe) 1) `shouldBe` (supprimerBloc 2 1 $ supprimerBloc 0 1 $ supprimerBloc 1 1 grilleComplexeMaybe, 3)
        it "Marque et supprime les trois rouges depuis le centre" $ do
            (parcourir Rouge 1 1 (supprimerBloc 1 1 grilleComplexeMaybe) 1) `shouldBe` (supprimerBloc 2 1 $ supprimerBloc 0 1 $ supprimerBloc 1 1 grilleComplexeMaybe, 3)
        it "Marque et supprime les sept bleus" $ do
            (parcourir Bleu 0 0 (supprimerBloc 0 0 grilleComplexeMaybe) 1) `shouldBe` (grilleBleuMaybe, 7)
    describe "Parcourir à partir de…" $ do
        it "Marque seulement le vert sans le supprimer" $ do
            (parcourirGrilleAPartirDe 0 2 grilleComplexeSansMaybe) `shouldBe` (modifierElement (const (True , Vert)) 0 2 grilleComplexeSansMaybe, 0)
        it "Marque seulement les rouges sans le supprimer" $ do
            (parcourirGrilleAPartirDe 0 1 grilleComplexeSansMaybe) `shouldBe` (modifierElement (const (True , Rouge)) 2 1 $ modifierElement (const (True , Rouge)) 1 1 $ modifierElement (const (True , Rouge)) 0 1 grilleComplexeSansMaybe, 0)
        it "Marque les bleus et les supprime" $ do
            (parcourirGrilleAPartirDe 0 0 grilleComplexeSansMaybe) `shouldBe` (grilleMarqueeSansBleu, 7)
    describe "simplifier grille marquée" $ do
        it "Supprime tous les bleus et donne le score" $ do
            (simplifierGrilleMarquee grilleComplexeSansMaybe 0) `shouldBe` (grilleSansBleuSansRouge, 11)
        it "Ne supprime pas les roses donc score 0" $ do
            (simplifierGrilleMarquee (map (map (\x -> (False, x))) $ mauvaiseGrille1) 0) `shouldBe` (mauvaiseGrille1, 0)
    describe "Simplifier grille" $ do
        it "Simplifie une grille avec des cranes" $ do
            (simplifierGrille grilleCrane) `shouldBe` (grilleCraneSimplifiee, 4)
    describe "Calculer le score" $ do
        it "Score grille1 bonne colonne" $ do
            (snd $ simplifierGrille (ajouterDeuxBlocs 4 Rose grille1)) `shouldBe` 4
        it "Score grille1 mauvaise colonne" $ do
            (snd $ simplifierGrille (ajouterDeuxBlocs 5 Rose grille1)) `shouldBe` 0
    describe "Choisir colonne" $ do
        it "Grille example 1" $ do
            (choisirColonne Rose grille1) `shouldBe` 4


grilleComplexe :: [[Couleur]]
grilleComplexe = [[Bleu, Rouge, Vert], [Bleu, Rouge], [Bleu, Rouge, Bleu, Rouge], [Bleu, Bleu, Bleu], [], []]

grilleComplexeMaybe :: [[(Bool, Maybe Couleur)]]
grilleComplexeMaybe = map (map (\couleur -> (False, Just couleur))) grilleComplexe

grilleComplexeSansMaybe :: [[(Bool, Couleur)]]
grilleComplexeSansMaybe = map (map (\couleur -> (False, couleur))) grilleComplexe

grilleBleuMaybe :: [[(Bool, Maybe Couleur)]]
grilleBleuMaybe = supprimerBloc 0 0 $ supprimerBloc 1 0 $ supprimerBloc 2 0 $ supprimerBloc 3 0 $ supprimerBloc 3 1 $ supprimerBloc 3 2 $ supprimerBloc 2 2 grilleComplexeMaybe

grilleSansBleu :: [[Couleur]]
grilleSansBleu = [[Rouge, Vert], [Rouge], [Rouge, Rouge], [], [], []]

grilleSansBleuSansRouge :: [[Couleur]]
grilleSansBleuSansRouge = [[Vert], [], [], [], [], []]

grilleMarqueeSansBleu :: [[(Bool, Couleur)]]
grilleMarqueeSansBleu = map (map (\couleur -> (False, couleur))) grilleSansBleu

grille1 :: [[Couleur]]
grille1 = [[], [], [Jaune, Jaune], [Rouge, Rouge], [Rose, Rose], [Bleu, Bleu]]

mauvaiseGrille1 :: [[Couleur]]
mauvaiseGrille1 = [[], [], [Jaune, Jaune], [Rouge, Rouge], [Rose, Rose], [Bleu, Bleu, Rose, Rose]]

grilleCrane :: [[Couleur]]
grilleCrane = [[Crane], [], [Crane, Crane], [Rouge], [Rouge, Rouge, Rouge], []]

grilleCraneSimplifiee :: [[Couleur]]
grilleCraneSimplifiee = [[Crane], [], [Crane], [], [], []]

import Test.Hspec

import qualified Data.Map as M
import Lib

main :: IO ()
main = hspec $ do
    describe "Hauteur de la grille" $ do
        it "Pour une grille vide, la première ligne vide de la colonne 1 est 1" $ do
            (premiereLigneVide (M.fromList []) 1) `shouldBe` 1
        it "Pour une colonne avec un élément, la première ligne vide est 2" $ do
            (premiereLigneVide (M.fromList [((1, 1), Bleu)]) 1) `shouldBe` 2
        it "Pour une colonne 1 avec un élément, la première ligne vide de la colonne 2 est 1" $ do
            (premiereLigneVide (M.fromList [((1, 1), Bleu)]) 2) `shouldBe` 1
    describe "Ajout de bloc" $ do
        it "Si on ajoute un bloc, la première ligne vide doit être 2" $ do
            (premiereLigneVide (ajouter (M.fromList []) 1 Bleu) 1) `shouldBe` 2
        it "Si on ajoute deux blocs, la première ligne vide doit être 3" $ do
            (premiereLigneVide (ajouterDeuxBlocs (M.fromList []) 1 Bleu) 1) `shouldBe` 3

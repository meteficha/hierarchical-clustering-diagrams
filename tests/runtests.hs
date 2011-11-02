-- from base
import System.Environment (getArgs)

-- from hierarchical-clustering
import Data.Clustering.Hierarchical

-- from diagrams-lib
import Diagrams.Prelude

-- from diagrams-cairo
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.CmdLine (multiMain)

-- from hspec
import Test.Hspec.Monadic (hspecX, describe, it)
import Test.Hspec.HUnit ()

-- from HUnit
import Test.HUnit ((~?=))

-- from this package
import qualified Diagrams.Dendrogram as D



main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> testsMain
    _  -> diaMain


testsMain :: IO ()
testsMain = hspecX $ do
  describe "fixedWidth" $ do
    it "works on a test example" $
       D.fixedWidth 1 test ~?=
          ( Branch 5
              (Branch 2
                (Branch 1
                  (Leaf 0.5)
                  (Leaf 1.5))
                (Leaf 2.5))
              (Leaf 3.5)
          , 4)

  describe "variableWidth" $ do
    let r :: Double -> Diagram Cairo R2
        r w = rect w 40
    it "works on a test example with fixed widths" $
       fst (D.variableWidth (const $ r 1) test) ~?=
          Branch 5
            (Branch 2
              (Branch 1
                (Leaf 0.5)
                (Leaf 1.5))
              (Leaf 2.5))
            (Leaf 3.5)
    let test2 = fmap f test
        f 'A' = 5
        f 'B' = 3
        f 'C' = 10
        f 'D' = 1
        f _   = undefined
    it "works on a test example with variable widths" $
       fst (D.variableWidth r test2) ~?=
          Branch 5
            (Branch 2
              (Branch 1
                (Leaf 2.5)
                (Leaf 6.5))
              (Leaf 13))
            (Leaf 18.5)




diaMain :: IO ()
diaMain =
    multiMain $ [ ("test", D.dendrogram D.Variable char test # lw 0.1) ] ++
                [ ("alpha-" ++ n, D.dendrogram D.Fixed char (alpha l) # lw 0.1)
                  | (n,l) <- [ ("single",   SingleLinkage)
                             , ("complete", CompleteLinkage)
                             , ("clink",    CLINK)
                             , ("upgma",    UPGMA)
                             ]
                ]


char :: Char -> Diagram Cairo R2
char c = pad 1.3 $ roundedRect (1,1) 0.1 `atop` text [c]

test :: Dendrogram Char
test = Branch 5
         (Branch 2
           (Branch 1
             (Leaf 'A')
             (Leaf 'B'))
           (Leaf 'C'))
         (Leaf 'D')

alpha :: Linkage -> Dendrogram Char
alpha link = dendrogram link ['A'..'Z'] dist
    where
      dist a b = fromIntegral $ abs (fromEnum a - fromEnum b)
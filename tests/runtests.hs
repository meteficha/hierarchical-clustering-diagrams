-- from hierarchical-clustering
import Data.Clustering.Hierarchical

-- from diagrams-lib
import Diagrams.Prelude

-- from diagrams-cairo
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Cairo.CmdLine (multiMain)

-- from this package
import qualified Diagrams.Dendrogram as D

main :: IO ()
main = multiMain $ [ ("test", D.dendrogram char test # lw 0.1) ] ++
                   [ ("alpha-" ++ n, D.dendrogram char (alpha l) # lw 0.1)
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
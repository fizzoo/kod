import           Test.Tasty
import           Test.Tasty.QuickCheck

main :: IO ()
main = defaultMain props

props :: TestTree
props = testGroup "Quickcheck props" []

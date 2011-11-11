import Test.QuickCheck
import Utility
import Data.Word (Word32)

prop_toWord_reversible :: Word32 -> Bool
prop_toWord_reversible x = toWord32 (toWord8 x) == x

main = do
    quickCheck prop_toWord_reversible

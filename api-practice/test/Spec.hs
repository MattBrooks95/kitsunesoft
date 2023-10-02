import Test.HUnit
import Test.Utils.LoadEnv (
    testParse
    )

main :: IO ()
main = do
    runTestTT testParse
    return ()


module LambdaTest where
import Language.Lambda
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Text.Parsec
import Test.QuickCheck.Checkers
import Debug.Trace.Helpers
import Debug.Trace

main = defaultMainWithArgs tests ["-a 15", "-o 4"]

--main = quickCheck prop_parse_is_inverse_of_ppr

tests = [
        testGroup "Parse Var" [
            testCase "test_parse_var_0" test_parse_var_0
            --testCase "test_parse_var_1" test_parse_var_1
        ],
        testGroup "Parse Lam" [
            testCase "test_parse_lam_0" test_parse_lam_0
            --testCase "test_parse_lam_1" test_parse_lam_1
        ],
        testGroup "Parse App" [
            testCase "test_parse_app_0" test_parse_app_0
            --testCase "test_parse_app_1" test_parse_app_1,
            --testCase "test_parse_app_2" test_parse_app_2
        ],
        testGroup "Complex Expression" [
            --testCase "test_parse_exp_0" test_parse_exp_0,
            testCase "test_parse_exp_1" test_parse_exp_1
            --testProperty "prop_parse_is_inverse_of_ppr" prop_parse_is_inverse_of_ppr
        ],
        testGroup "whnf" [
            testCase "whnf_test_0" whnf_test_0,
            testCase "whnf_test_1" whnf_test_1,
            testCase "whnf_test_2" whnf_test_2,
            testProperty "prop_whnf" prop_whnf
        ],
        testGroup "whnf" [
            testCase "nf_test_0" nf_test_0,
            testCase "nf_test_1" nf_test_1,
            testCase "nf_test_2" nf_test_2,
            testProperty "prop_nf" prop_nf
        ]     
    ]
    
test_parse_var_0 = actual @?= expected where
    (Right actual) = runParser parse_var () "" "x"
    expected = Var "x"
    
--test_parse_var_1 = actual @?= expected where
--    (Right actual) = runParser parse_var () "" "(x)"
--    expected = Var "x"
    
test_parse_lam_0 = actual @?= expected where
    result = runParser parse_lambda () "" "\\x.x"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = Lam "x" $ Var "x"

{- 
test_parse_lam_1 = actual @?= expected where
    result = runParser parse_lambda () "" "(\\x.x)"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = Lam "x" $ Var "x" 
-}   
    
test_parse_app_0 = actual @?= expected where
    result = runParser parse_app () "" "x y"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = App (Var "x") $ Var "y"

{-
test_parse_app_1 = actual @?= expected where
    result = runParser parse_app () "" "(x y)"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = App (Var "x") $ Var "y"

  
test_parse_app_2 = actual @?= expected where
    result = runParser parse_app () "" "((x) y)"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = App (Var "x") $ Var "y"

    
test_parse_exp_0 = actual @?= expected where
    actual = [expr| ((\x.x) y) |]
    expected = App (Lam "x" $ Var "x") $ Var "y"
-}

test_parse_exp_1 = actual @?= expected where
    result = runParser parse_expr () "" "((\\x.x) y)"
    actual = case result of
        Right x -> x
        Left x -> error $ show x
    expected = App (Lam "x" $ Var "x") $ Var "y"
    
run_p = runParser parse_expr () ""
    
prop_parse_is_inverse_of_ppr :: Expr -> Bool
prop_parse_is_inverse_of_ppr x = result where
    parsed = runParser parse_expr () "" $ ppr_lambda x
    result = case parsed of
        Right e -> e == x
        Left _ -> trace (show x) False 
        
  
whnf_test_0 = actual @?= expected where
    expected = [expr|y|]
    actual   = whnf [expr|((\x.x) y)|]
    
whnf_test_1 = actual @?= expected where
    expected = [expr|(\z.(\x.x) z)|]
    actual   = whnf [expr|(\z.(\x.x) z)|]

whnf_test_2 = actual @?= expected where
    expected = [expr|(\z.(\x.x) (\x.x))|]
    actual   = whnf [expr|(\z.(\x.x) (\x.x))|]

is_in_whnf :: Expr -> Bool
is_in_whnf (Lam _ _) = True
is_in_whnf (App (Var _) _) = True
is_in_whnf (Var _)   = True
is_in_whnf _ = False

prop_whnf :: Expr -> Bool
prop_whnf = is_in_whnf . whnf 

nf_test_0 = actual @?= expected where
    expected = [expr|y|]
    actual   = nf [expr|((\x.x) y)|]
    
nf_test_1 = actual @?= expected where
    expected = [expr|(\z.z)|]
    actual   = nf [expr|(\z.(\x.x) z)|]
    
nf_test_2 = actual @?= expected where
    expected = [expr|(\z.(\x.x z))|]
    actual   = whnf [expr|(\z.(\x.x) (\x.x z))|]


is_in_nf :: Expr -> Bool
is_in_nf (Lam _ x) = is_in_nf x
is_in_nf (App (Var _) _) = True
is_in_nf (App x y) = (is_in_nf x) && (is_in_nf y)
is_in_nf (Var _)   = True
is_in_nf _ = False

prop_nf :: Expr -> Bool
prop_nf = is_in_nf . nf

--should make some tests for the sub function
--just make sure that sub works
--makes a test for nf whnf
















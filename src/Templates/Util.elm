module Templates.Util exposing (mChain, mChainMaybe, mChainResult, safeCCL, safeCCU, safeName)

import Elm.CodeGen as CG exposing (Expression)
import Set
import String.Case as Case


{-| Checks if a name matches an Elm keyword, and proposes a different name to
use instead, which is the original with an underscore appended.

    cleanupName "type" == "type_"

-}
safeName : String -> String
safeName val =
    let
        keywords =
            Set.fromList
                [ "type"
                , "alias"
                , "let"
                , "in"
                , "if"
                , "then"
                , "else"
                , "import"
                , "exposing"
                , "module"
                , "as"
                , "case"
                , "of"
                , "port"
                , "Int"
                , "Bool"
                , "Float"
                , "String"
                , "Char"
                , "Order"
                , "Never"
                ]
    in
    if Set.member val keywords then
        val ++ "_"

    else
        val


safeCCL : String -> String
safeCCL =
    Case.toCamelCaseLower >> safeName


safeCCU : String -> String
safeCCU =
    Case.toCamelCaseUpper >> safeName


mChain : (Expression -> Expression) -> Expression -> List Expression -> Expression
mChain lift head expressions =
    List.map lift expressions
        |> CG.pipe head


mChainResult : Expression -> List Expression -> Expression
mChainResult =
    mChain liftResult


mChainMaybe : Expression -> List Expression -> Expression
mChainMaybe =
    mChain liftMaybe


liftResult : Expression -> Expression
liftResult expr =
    CG.apply [ CG.fqFun resultMod "andThen", expr ]


liftMaybe : Expression -> Expression
liftMaybe expr =
    CG.apply [ CG.fqFun maybeMod "andThen", expr ]


maybeMod : List String
maybeMod =
    [ "Maybe" ]


resultMod : List String
resultMod =
    [ "Result" ]

module Templates.Helper exposing (mChain, mChainMaybe, mChainResult)

import Elm.CodeGen as CG exposing (Expression)
import Set
import String.Case as Case


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

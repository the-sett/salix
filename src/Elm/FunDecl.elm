module Elm.FunDecl exposing (FunDecl, funDeclAsExpression, funDeclAsLetDecl, funDeclAsTopLevel)

{-| FunDecl captures everything needed to make a function declaration either at
top-level or inside a let block. This allows the decision to build a function as
a top-level or a let expression to be deferred, which allows the same code to be
used to generate both.
-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)



--== Function Declarations


type alias FunDecl =
    { doc : Maybe (Comment DocComment)
    , sig : Maybe TypeAnnotation
    , name : String
    , args : List Pattern
    , impl : Expression
    }


funDeclAsTopLevel : FunDecl -> Declaration
funDeclAsTopLevel funDecl =
    CG.funDecl funDecl.doc funDecl.sig funDecl.name funDecl.args funDecl.impl


funDeclAsLetDecl : FunDecl -> LetDeclaration
funDeclAsLetDecl funDecl =
    CG.letFunction funDecl.name funDecl.args funDecl.impl


funDeclAsExpression : FunDecl -> Expression
funDeclAsExpression funDecl =
    funDecl.impl

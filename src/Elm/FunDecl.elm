module Elm.FunDecl exposing
    ( FunGen, FunDecl
    , Options, defaultOptions
    , asTopLevel, asLetDecl
    , asExpression
    )

{-| FunDecl captures function declarations along with any associated linkage required.
Everything needed to make a function declaration either at top-level or inside a let block,
or to extract the function body as an expression is captured as a `FunDecl`.

`Options` can be applied when extracting a `FunDecl` as generated code; to rename it,
to include or exclude it from the exposings, to modify or disable its docs, to include
a function signature or not.

This allows the decision to build a function as a top-level, a let expression or to extract it
as an expression for further manipulation, to be deferred.

Code that writes function can therefore be used in many different ways as a result, and there
is no need to write specialized cases for all the different ways in which a code generated
function can be used.

@docs FunGen, FunDecl

@docs Options, defaultOptions

@docs asTopLevel, asLetDecl

-}

import Elm.CodeGen as CG exposing (Comment, Declaration, DocComment, Expression, Import, LetDeclaration, Linkage, Pattern, TypeAnnotation)
import Maybe.Extra


type alias Options =
    { name : Maybe String
    , inExposings : Bool
    , includeSig : Bool
    , includeDoc : Bool
    , altDoc : Maybe (Comment DocComment)
    }


defaultOptions =
    { name = Nothing
    , inExposings = True
    , includeSig = True
    , includeDoc = True
    , altDoc = Nothing
    }


type alias FunBuilder =
    Options -> FunGen



--== Function Generators


type alias FunGen =
    ( FunDecl, Linkage )


type alias FunDecl =
    { doc : Maybe (Comment DocComment)
    , sig : Maybe TypeAnnotation
    , name : String
    , args : List Pattern
    , impl : Expression
    }


applyOptions : Options -> FunGen -> FunGen
applyOptions options ( funDecl, ( imports, exposings ) ) =
    let
        sig =
            if options.includeSig then
                funDecl.sig

            else
                Nothing

        doc =
            if options.includeDoc then
                Maybe.Extra.or options.altDoc funDecl.doc

            else
                Nothing

        name =
            options.name |> Maybe.withDefault funDecl.name

        linkage =
            ( imports
            , if options.inExposings then
                exposings

              else
                []
            )
    in
    ( { doc = doc, sig = sig, name = name, args = funDecl.args, impl = funDecl.impl }
    , linkage
    )


{-| Generates a `Declaration` for a function.
-}
asTopLevel : Options -> FunGen -> ( Declaration, Linkage )
asTopLevel options funGen =
    applyOptions options funGen
        |> (\( fd, linkage ) -> ( CG.funDecl fd.doc fd.sig fd.name fd.args fd.impl, linkage ))


{-| Generates a `LetDeclaration` for a function.

Note: As let declaractions are not visible outside of their scope, these are
never exposed, so the `inExposings` option is always set to `False` when using
this.

A let declaration will not have any docs or a type signature either.

-}
asLetDecl : Options -> FunGen -> ( LetDeclaration, Linkage )
asLetDecl options funGen =
    applyOptions { options | inExposings = False } funGen
        |> (\( fd, linkage ) -> ( CG.letFunction fd.name fd.args fd.impl, linkage ))


{-| Generates an `Expression` for a function.

Note: As the expression has not been assigned to a name, it cannot be visible
outside of the module. For this reason the `inExposings` option is always set
to `False` when using this. If you decide to expose the function you must add
whatever name you give it back into the linkage.

An expression will not have any docs or a type signature either.

-}
asExpression : Options -> FunGen -> ( Expression, Linkage )
asExpression options funDecl =
    applyOptions options funDecl
        |> (\( fd, linkage ) -> ( fd.impl, linkage ))

**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org


# Salix

Salix is a data modeling language that is designed to support code generation in across many different situations. It aims to provide a fairly rich data modeling capability that can be used as the foundation to generate functional, object-oriented, database code, data interchange formats or remote APIs and their specifications.

This is currently a work in progress, and is not likely to be of use to you, but feel free to look around if you are inquisitive; this is currently being used to generate Elm stubs for AWS services and you can learn from this how that is done.

The modeling language is split into 3 layers. Layer 1 deals with the pure modeling language. Layer 2 deals with checking that the model is well-formed and deriving information from it that is potentially of use for code generation without specifically targeting any particular code generator. Layer 3 deals with attaching additional predicates to the model that enable specific code generators to work with the model to understand how to generate from it.

The name Salix comes from the Willow tree, which is a fast growing tree - code generators help you rapidly build code syntax trees.

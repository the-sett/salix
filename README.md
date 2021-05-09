**Contacts for Support**
- @rupertlssmith on https://elmlang.slack.com
- @rupert on https://discourse.elm-lang.org

**Status** - 09-May-2021

This is a work in progress, and is not likely to be of use to you, but feel free to look around if you are inquisitive.

This is currently being used to generate Elm stubs for AWS services and you can learn from this how that is done:

https://github.com/the-sett/elm-aws-codegen


# Salix

Salix is a data modelling language that is designed to support code generation across many different situations. It aims to provide a fairly rich data modelling capability that can be used as the foundation to generate functional, object-oriented, database code, data interchange formats or remote APIs and their specifications.

The modelling language is split into 3 layers. Layer 1 deals with the pure modelling language. Layer 2 deals with checking that the model is well-formed and deriving information from it that is potentially of use for code generation without specifically targeting any particular code generator. Layer 3 deals with attaching additional properties to the model that enable specific code generators to work with the model to understand how to generate from it.

The name Salix comes from the Willow tree, which is a fast growing tree - code generators help you rapidly build code syntax trees.

# Release Notes

**1.0.0**
- Overall structure and language levels developed. Some templates for
generating Elm code are provided.

**2.0.0**
- Mainly the error reporting aspect has been developed.
- APIs to standardize the L2 and L3 processors were also placed around those. This makes the system more modular allowing many different implementations to be
'plugged in'. There is still some work to do in this area, around standardizing
how code generator outputs work.

**3.0.1**
- L3 code generator for JSON codings in Elm introduced.
- A few small changes to error reporting when querying properties.
- Added the L1.Basic type to RCBasic.

**4.0.0**
- Removed `Properties` from `Type`s. Properties will only be allowed on declarations and fields. This will make the text source language more manageable as allowing properties anywhere will make it quite awkward. Properties were allowed on types in order to be able to force a property to be set in particular places. This will be handled instead by enriching the property descriptors - not in this version.

**4.0.1**
- Bug fixes to JSON codecs:
  Added dict encoder for basic key types.
  Basic encoder functions don't need a 'val' argument.
  Importing Decoder from Json.Decode.

**4.0.2**
- Bumped elm-css version to remove dependency on Skinney/murmur3.

# Google Closure Templates for Haskell

This project is an attempt to bring the Closure Templates to the Haskell
platform. It provides a parser and a renderer and doesn't depend on the Google
libraries

## Unimplemented features

* Line joining: In the current implementation, whitespace is preserved as is.
  This will be configurable.
* custom print directives
* The `{msg}`, `{css}` and `{del*}` tags
* Translation and Bidi support
* Contextual autoescaping (currently only `autoescape="true/false"` is
  implemented)
* Soy Doc (will probably never be implemented)

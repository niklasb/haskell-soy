# Google Closure Templates for Haskell

This project is an attempt to bring the Closure Templates to the Haskell
platform. It provides a parser and a renderer and doesn't depend on the Google
libraries.

The initial code was written during an internship at [factis research][1]. They
also generously offered to continue sponsoring this project, as they like to
give back to the open source community. Thanks a lot!

## Unimplemented features

* The `{del*}` tags
* The `{css}` tag (currently the inner text is simply echoed)
* Translation and Bidi support (currently `{msg}` just echoes the contained text)
* Contextual autoescaping (currently only `autoescape="true/false"` is
  implemented)
* Soy Doc (will probably never be implemented)

[1]: http://www.factisresearch.com/

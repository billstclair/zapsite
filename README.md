[Zapsite](https://zapsite.ninja/) is a simple web site editor. It is written in [Elm](https://elm-lang.org/).

This is a work in progress.

For development:

    cd ~/.../zapsite
    elm reactor
    
Then aim your browser at http://localhost:8000/site/index.html

After code changes, use `bin/build` to create `site/elm.js`, then reload the browser window.

To test `Zapsite.EncodeDecode`:

    cd ~/.../zapsite
    elm-test

The table formatting is in [site/css/site.css](./site/css/site.css)

Bill St. Clair, 9 April 2024

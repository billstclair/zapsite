# Templates

Zapsite is a templating system. The templates are Markdown, with
tables and {fillins}. The users specify the {fillins} for each page,
and Zapsite creates the pages with the {fillins} filled in.

Elm's builtin Markdown doesn't do tables, so I have to work around
that somehow. Hopefully, it won't require an entirely new Markdown
processor.

This should do what I need:

https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown.PrettyTables


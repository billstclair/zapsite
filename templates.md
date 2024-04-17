# Templates

Zapsite is a templating system. The templates are Markdown, with
tables and {fillins}. The users specify the {fillins} for each page,
and Zapsite creates the pages with the {fillins} filled in.

Elm's builtin Markdown doesn't do tables, so I have to work around
that somehow. Hopefully, it won't require an entirely new Markdown
processor.

This should do what I need:

https://package.elm-lang.org/packages/matheus23/elm-markdown-transforms/latest/Markdown.PrettyTables

# Definition

A template is Markdown text, with "{ <name> }" references. Each of the
<name>s is filled in at rendering time. If a reference is in a table
row, there can be more than one set of key/value pairs to fill in the
table.

    
Template: "/index"

## Stoneder.club

Cheech: "I'm stoneder than you."
Chong: "Wow, man. Cool."

[places](places/) [online](online/)

[GitHub](https://github.com/billstclair/stoneder)


Template: "/places/index"

## places - [stonedir.club](../)

Places to buy and/or imbibe.

state | city
----- | ----
{state} | [{city}]({state}-{city})


Template: "/places/tennessee-knoxville"

## Knoxville, Tennessee - [places](index) - [stoneder.com](../../)

place | imbibe | purchase | address | phone
----- | ------ | -------- | ------- | -----
{place} | {imbibe} | {purchase} | {address} | {phone}


Template: "/online/index"

## online - [stoneder.club](../)

place | description | address | email | phone
----- | ----------- | ------- | ----- | -----
{place} | {description} | {address} | {email} | {phone}

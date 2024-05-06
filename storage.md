# ZapSite Storage

ZapSite allows templates, var->val maps, and page->(template, maps) storage to be on the local server, managed by ZapSite's server, or in Amazon S3 (or Digital Ocean spaces). The web site creator can choose either at creation time, and change it later.

## Local Storage

Will use the standard `localStorage` JS library to store either in the browser's database or the server's file system (via Node.JS implementation of `localStorage`).

Changes to data in a `localStorage` database are propagated to viewers of affected pages. Changes are live on all screens.

Uses my `billstclair/elm-websocket-framework-server` technology for this.

```
get <key>
put <key> <value>
list <prefix>
```


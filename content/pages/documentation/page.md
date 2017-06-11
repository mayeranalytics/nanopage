![](documentation.svg)

This is the *nanoPage* documentation.

---

## Overview

*nanoPage* is a minimal static flat file CMS based on [Haskell](http://www.haskell.org) [Spock](htp://spock.li). It is suitable for small websites
or microsites with mostly static content. 
Content is stored as [Markdown](https://en.wikipedia.org/wiki/Markdown),
and edited offline using any Markdown editor. A *nanoPage* website is published by compiling both source code and static content. The resulting executable is fully self-contained with web-server *and* content. As such, it is fast, easy to deploy and easy to secure. A demo is running on [http://nanopage.li](http://nanopage.li).

*nanoPage* is covered by the very permissive MIT license. The source code
can be found on [github](https://github.com/mayeranalytics/nanoPage).

### Motivation
- easy to extend
- avoid heavy frameworks
- lightweight: Self-contained executable including all the static content
- @TODO

### Use-cases
- blogs
- static microsites
- embedded devices

### Features

#### @TODO
- templates
- partials
- preview
- tags
- categories
- meta-information such as: Author, keywords, description

### What *nanoPage* is not
*nanoPage* is *not* a multi-user WYSIWYG flat file CMS like [*Grav*](https://getgrav.org) or [*Ghost*](https://ghost.org/). The admin panel
is rather minimal, in particular it does not allow content editing. 
Instead, content is edited in a markdown editor of your choice. 

### Pros and cons
*nanoPage* was created for a narrow use-case, so it's worth clearly stating
the pros and cons:

#### Pros
- Easy to deploy because web-server and content are contained in the excutable.
- Easy to secure, because there is nothing to hack, essentially.
- Self-contained excutable means a small footprint
- Small code-base
- Fast
- Dynamic content can be inserted via [*Partials*](#partials). This makes *nanoPage* a good basis for single-page apps.
- Markdown

#### Cons
- No online editing
- Every deployment requires re-compilation
- Content resides in RAM, hence *nanoPage* is not suitable for websites with large static content.

<a name="CMS"></a>
## CMS

```bash
content/
├── Makefile
├── pages
├── static
└── templates
```

<a name="cms-templates"></a>
### Templates
Todo

<a name="cms-partials"></a>
### Partials 
Todo

## Running the server

To run the server the current working directory must be the content
directory, so a typical start looks like

```bash
./bin/nanopage
```

```text
This is nanoPage, a minimalistic flat-file CMS written in Haskell. For more
information see http://nanopage.li/.

Usage: nanopage [-p|--port INT] [-n|--server STRING] [-m|--mode MODE]
                [-C|--working-dir STRING]
  Run the nanoPage webserver.

Available options:
  -p,--port INT            port number (default: 3000)
  -n,--server STRING       Server name (default: "localhost")
  -m,--mode MODE           Server mode can be PROD or ADMIN. The admin pages are
                           only shown in ADMIN mode. (default: PROD)
  -C,--working-dir STRING  Working directory (default: ".")
  -h,--help                Show this help text
```

The command line options are:

- `port`: The port number, default is `3000`.
- `server`: The server name, default is `localhost`.
- `mode`: The server mode identifies the deployment environment, it be `ADMIN` or `PROD`. `PROD` is the default. The admin pages can only be seen in the `ADMIN` mode.
- `content`: Before running the server change the working directory to this file path.

## Editing content

Since all content is embedded in the executable content has to be edited
offline. The server is run locally in admin mode by providing the 
`-m ADMIN` flag.

```bash
cd contents; ../bin/nanopage -m ADMIN   # or
./bin/nanopage -m ADMIN -C contents
```


## Internals

The source code can be found on [github](https://github.com/mayeranalytics/nanoPage).

```bash
app/
├── Makefile
├── src/
│   ├── FileDB.hs
│   ├── Internal/
│   │   ├── FileDB.hs
│   │   ├── Helpers.hs
│   │   └── HtmlOps.hs
│   ├── Main.hs
│   ├── Page.hs
│   ├── Partials/
│   │   ├── AdminBlock.hs
│   │   ├── CategoryList.hs
│   │   ├── KeywordList.hs
│   │   ├── TagCloud.hs
│   │   └── TagList.hs
│   └── Sitemap.hs
├── nanopage.cabal
└── stack.yaml
```

### Partials
Partials are located in `app/src/Partials`. A partial has 
to implement class `Partial` defined in `app/src/Internal/Partial.hs`.

```haskell
class Partial a where
    extraRoutes :: a -> [Sp.SpockM FileDB () () ()]
    partial     :: a -> FileDB -> Page -> Params -> H.Html
```

## Resources

- [Google on SEO](https://support.google.com/webmasters/answer/79812?hl=en)
- [Favicon checker](https://realfavicongenerator.net/favicon_checker)

### Markdown editors

#### macOS X

[MacDown](https://macdown.uranusjr.com/) is a good, opensource Markdown editor
for macOS X. It has a customizable renderer, syntax highlighting, 
auto-completion.

[Mou](http://25.io/mou/) has a checkered past and was resurrected in a 
[Indiegogo](https://www.indiegogo.com/projects/mou-1-0-markdown-editor-on-os-x-for-you) campaign. macOS Sierra is still not supported.

Many editors have plugins that provide a good editing experience. 

[Atom](https://atom.io/) with the [markdown-preview-plus](https://atom.io/packages/markdown-preview-plus) package works very well. Then [markdown-preview-enhanced](https://atom.io/packages/markdown-preview-enhanced) package looks promising but is still in beta.

#### Linux

todo

#### Windows

todo
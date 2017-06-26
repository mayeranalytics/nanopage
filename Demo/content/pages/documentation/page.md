![](documentation.svg)

This is the *nanoPage* documentation.

---

## Overview

*nanoPage* is a minimal static flat file CMS based on [Haskell](http://www.haskell.org) [Spock](htp://spock.li). It is suitable for small websites
or microsites with mostly static content. 
Content is stored as [Markdown](https://en.wikipedia.org/wiki/Markdown), and edited offline using any Markdown editor. Publication of a *nanoPage* website requires compiling the source code. The resulting executable is fully self-contained, i.e. it contains the web-server *and* all the content. In order to deploy a website just copy the executable to the server and run it. Since there is no database and no file system *nanoPage* is very secure. 

A demo is running on [http://nanopage.li](http://nanopage.li).

*nanoPage* is covered by the very permissive MIT [license](https://github.com/mayeranalytics/nanoPage/blob/master/nanopage/LICENSE). The source code can be found on [github](https://github.com/mayeranalytics/nanoPage).

### Motivation
- easy to extend
- avoid heavy frameworks
- lightweight: Self-contained executable including all the static content

### Use-cases
- blogs
- static microsites
- embedded devices

### Features

- Templates
- Partials
- Preview cards
- Tags
- Categories
- Meta-information such as: Author, keywords, description

### What *nanoPage* is not
*nanoPage* is *not* a multi-user WYSIWYG flat file CMS like [*Grav*](https://getgrav.org) or [*Ghost*](https://ghost.org/). The admin panel is rather minimal, in particular it does not allow content editing. Instead, content is edited offline in a markdown editor of your choice. 

### Pros and cons
*nanoPage* was created for a narrow use-case, so it's worth clearly stating
the pros and cons:

#### Pros

- Easy to deploy because web-server *and* content are contained in the excutable. A typical executable has a size of <20MB (after [upx](https://upx.github.io/) compression).
- Easy to secure, because there is nothing to hack, essentially
- Self-contained excutable means a small footprint
- Small code-base - the core is about 1000 lines of Haskell.
- Fast
- Dynamic content can be inserted via [*Partials*](#partials). This makes *nanoPage* a good basis for single-page apps.
- Markdown

#### Cons
- No online editing
- Every deployment requires re-compilation and copying of the executable
- Content resides in RAM, hence *nanoPage* is not suitable for websites with large static content.

### Why Markdown?

When it comes to content editing in a CMS there are effectively three choices. Straight HTML gives you total control, but structure elements such as lists and tables are cumbersome. Then there are the [wysiwyg](https://wordpress.org/plugins/tags/wysiwyg/) editors such as *TinyMCE* or *CKEditor*. They are user friendly but require clicking around in a GUI which slows down the editing process. Lastly, there are the markup languages such as *BBEdit* and *Markdown*. They strike a good balance between typing speed and flexibility. In the case of *Markdown* there are a lot of excellent editors available, free and for-pay, and it is easy to learn. See the demo page on [nanopage.li](http://nanoplage.li/demo).  *Markdown* is also used for editing user content on sites like [Stackexchange](https://stackexchange.com/) or [Github](https://github.com/). From an implementation point of view, *Markdown* is supported by the wonderful [pandoc](https://hackage.haskell.org/package/pandoc) package which opens many possibilities for extending the standard *Markdown* syntax to cover some flavours that are supported by some of the Editors. See the section ["Markdown Editors"](#markdown-editors) below for more.

<a name="CMS"></a>
## Getting Started

#### Download

We'll assume you have the Haskell Tool Stack installed on your system. If not, follow the instructions on [haskellstack.org](https://docs.haskellstack.org/en/stable/README/).

First, download *nanoPage* from the repository on [github](https://github.com/mayeranalytics/nanoPage/).

```bash
cd some/good/location
git clone git@github.com:mayeranalytics/nanoPage.git
```

#### Build

Next build the demo app 

```bash
make Demo	# or
APPDIR=$PWD/Demo stack --local-bin-path Demo/bin/ install
```

The environment variable `APPDIR` tells the compiler where the content directory can be found (absolute paths needed!). The stack option `—local-bin-path` tells the linker to install the executable in the standard location `bin/` underneath the `APPDIR`. Compilation of the *nanoPage* source code may take a minute or two, and when building it the *first time* Stack will also download and install all required packages which will take additional time. Normally you have to compile *nanoPage* only once to be able to run it in admin mode.

#### Run the demo app in admin mode

In admin mode files are served from the local `content/` folder.

```bash
Demo/bin/demo-app -m ADMIN -C Demo/content
```

The app will automatically open the browser for you and navigate to `http://localhost:3000`. See [Running the Server](#running-the-server) below for more details.

#### Deploy the app

The executable is fully self contained, so all there is to do is copy it to the server and run it.

## The CMS

### File Structure

When you download *nanoPage* from [github](https://github.com/mayeranalytics/nanoPage/) you essentially obtain the following file tree (files like Readme and Makefile are omitted).

```bash
.
├── Demo/
│   ├── Demo.cabal
│   ├── Main.hs
│   ├── bin/
│   └── content
│       ├── pages/
│       ├── static/
│       └── templates/
├── nanopage/
└── stack.yaml
```

The example application is called 'Demo' and is located in the top-level folder with the same name. Within the application folder `Demo/` you have the `.cabal` file for a single executable, in this case named `demo-app`. The code for running *nanoPage* ist quote short:

```haskell
-- Main.hs
module Main where
import Nanopage

main :: IO ()
main = do
    opts <- parseCliOpts      -- command line options give config
    runNanopage opts          -- run app with config
```

#### The `content/` folder

Each page in has its own folder in `content/pages/`. Templates are placed in the `templates/` folder. Static content such as style sheets and javascript files go into the `static/` folder.

<a name="cms-add-a-page"></a>

### Adding pages

1. Create a directory in `content/pages/`. The directory name can serve as the slug, but you can also define the slug in the `config.yaml` file. Let's assume the directory is called `my-new-page/`.
2. Create a markdown file in `content/pages/my-new-page/`, the name of the file identifies the template to use. For example, `page.md` will use the template `content/templates/page.html`.
3. Create a `config.yaml` file in `content/pages/my-new-page/` and specify title, author, slug, etc. See the section "[The config file](#cms-config-file)" below. 
4. Edit the markdown file in the `content/pages/my-new-page/` and keep all assets such as images, videos, etc. in this directory or in the `static/` directory.
5. Run *nanoPage* locally in admin mode. I.e. execute `bin/nanopage -m ADMIN -C content`. In admin mode *nanoPage* will serve files directly from the file system.
6. To publish the new page compile the executable (`make`), optionally compress it (`make compress`), test it  and finally copy it to the server where it should be run in production mode `bin/nanopage`. See "[Running the server](#running-the-server)" for details.

<a name="cms-config-file"></a>

### The config file

The config file contains information such as page title, slug, author, etc. It is located in the page directory `content/pages/<page-name>/` and must have the file name `config.yaml`.

```yaml
title: My New Page
slug: my-new-page
keywords: [A, list, of, keywords]
tags: [A, list, of, tags]
categories: [A, list, of, categories]
author: A. Name
```

Todo

<a name="cms-templates"></a>

### Templates
Templates reside in the `content/templates` folder. A template is simply an html file with [Mustache](https://mustache.github.io/) patterns. The Mustache patterns are used to include basic features and [partials](#cms-partials). For example, the information contained in the `config.yaml` file are available as, etc. Mustache template patterns follow the convention that double braces  entity-escape  its content whereas the triple braces do not.

- `title`
- `author`
- `keywords`
- `tags`
- `categories`
- `content`: Use triple braces, i.e. do not entity-escape the content.

Todo: Details

<a name="cms-partials"></a>
### Partials 
A partial  provides a feature. They are HTML snippets can provide potentially dynamic content. Partials are included in the markdown page via the Mustache syntax `partial-name` and `partial-name`. See the [Partials](#internals-partials) section below for more details. For example, in order to include the author name, as defined in `config.yaml`, just write in the markdown file.

```html
{{{author}}}
```

The partials provided in the config file are:

- `title`: The title of the page, used in the `<head>` section of a HTML template.
- `keywords`: A string of comma separated keywords, usually only used in the keywords meta tag of a HTML template. (The keywords meta tag is actually of little or no importance for SEO.)
- `description`: A string of comma separated keywords, usually only used in the description meta tag of a HTML template.
- `author`: The author name, used in the author meta tag of a HTML template, and in markdown pages themselves.

It is also possible to create partials with dynamic content.

- `CategoryList`
- `KeywordList`
- `TagCloud`
- `TagList`

Todo: More details

<a name="running-the-server"></a>

## Running the Server

To run the server the current working directory must be the content
directory, so a typical start looks like

```bash
Demo/bin/demo-app
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
cd Demo/contents; ../bin/nanopage -m ADMIN   # or
Demo/bin/nanopage -m ADMIN -C Demo/contents
```


## Internals

*nanoPage* uses the [Spock](https://hackage.haskell.org/package/Spock) web framework, [stache](https://hackage.haskell.org/package/stache) for Mustache templates, [pandoc](https://hackage.haskell.org/package/pandoc) for conversion from markup to html and [file-embed](https://hackage.haskell.org/package/file-embed) for embedding static content.

The source code can be found on [github](https://github.com/mayeranalytics/nanoPage).

```bash
nanopage/
├── Makefile
├── src
│   ├── CliOpts.hs       # parse command line options into NanopageConfig
│   ├── Config.hs        # defines NanopageConfig
│   ├── FileDB.hs        # FileDb manages the content
│   ├── Internal
│   │   ├── EmbedDir.hs  # compile-time embedding of files
│   │   ├── FileDB.hs    # definition of FileDB
│   │   ├── Helpers.hs   # some common utility functions
│   │   ├── HtmlOps.hs   # some common Blaze.Html operations
│   │   ├── Partial.hs   # definition of Partial
│   │   └── SpockExt.hs  # adds the xml function
│   ├── MkPartialsHs.hs  # read the Partials directories and create an import file
│   ├── Nanopage.hs      # defines runNanopage, re-exports functions
│   ├── Page.hs			# define Page
│   ├── Partials
│   │   ├── AdminBlock.hs
│   │   ├── CategoryList.hs
│   │   ├── KeywordList.hs
│   │   ├── TagCloud.hs
│   │   └── TagList.hs
│   ├── Partials.hs      # re-exports all Partials (created by mkPartialsHs)
│   └── Sitemap.hs       # creates a sitemap.xml
├── nanopage.cabal
└── stack.yaml
```

<a name="internals-partials"></a>

### Partials

Partials are located in `app/src/Partials`. A partial has 
to implement class `Partial` defined in `app/src/Internal/Partial.hs`.

```haskell
class Partial a where
    extraRoutes :: a -> [Sp.SpockM FileDB () () ()]
    partial     :: a -> FileDB -> Page -> Params -> Text.Blaze.Html
    partialName :: a -> T.Text
```

The three functions are:

##### `extraRoutes`

Some partials provide functionality that requires extra routes. The `TagCloud` partial, for example, defines the route `pages` which returns a json file with information about each page. Client-side Javascript then displays the information as a tag cloud.

##### `partial`

This function renders a partial from the inputs `FileDB`, `Page` and the `Params`:

- `FileDB`
- `Page` 
- `Params` is a type synomym for `[(Text, Text)]` which is the return type of  the request parameters, as returned by`Spock`'s [`params`](http://hackage.haskell.org/package/Spock-0.7.4.0/docs/Web-Spock-Shared.html#v:params) function.

##### `partialName`

This is the name of the partial, and this is the string by whicht the partial is identified in Mustache templates. 

## Resources

### SEO

- [Google on SEO](https://support.google.com/webmasters/answer/79812?hl=en)
- [Favicon checker](https://realfavicongenerator.net/favicon_checker)

<a name="markdown-editors"></a>

### Markdown editors

There is no lack of choice when it comes to Markdown editors. Many editors implement their own Markdown flavor which can make it just a little bit harder to get good results with *nanoPage*: Markdown may look really nice in the editor but may look less appealing when rendered by *nanoPage*'s renderer (pandoc).

The editor that stands out is [Typora](https://typora.io/). It differs from the other editors in that it does not have a preview editor. Instead the markdown is applied while typing, the result is a highly enjoyable editing experience. Mathjax, and other nifty features, are supported. (Note that inline equations have to be enabled in the settings.) *Typora* is still in beta and still free. The stable release will probably require a paid license. *Typora* is available for Linux, Windows and Mac.

#### General purpose editors with plugins

Many general purpose editors have plugins that provide a good editing experience. These editors are usually free and available across platforms.

[Atom](https://atom.io/) with the [markdown-preview-plus](https://atom.io/packages/markdown-preview-plus) package works very well. Then [markdown-preview-enhanced](https://atom.io/packages/markdown-preview-enhanced) package looks promising but is still in beta.

[Visual Studio Code](https://code.visualstudio.com) is a relative newcomer to the editor space, but it is quickly gaining traction.

[Sublime Text](https://www.sublimetext.com/) is a somewhat pricey text editor with a ton of plugins. Strangely, there seems to be no live preview *within* Sublime Text. Instead, a browser will be opened.

#### MacOS X

[MacDown](https://macdown.uranusjr.com/) is a good, opensource Markdown editor for macOS X. It has a customizable renderer, syntax highlighting, auto-completion, and more.

[Mou](http://25.io/mou/) has a checkered past and was resurrected in an [Indiegogo](https://www.indiegogo.com/projects/mou-1-0-markdown-editor-on-os-x-for-you) campaign. It's not free and macOS Sierra is still not supported. It's hard to come up with a reason to use it.

#### Linux

Linux is as blessed with choice as any platform. Of course the usual candidates [emacs](http:emacs.org) and [vim](http://www.vim.org/) can be used. Some extensions even allow preview, such as [markdown-mode](http://jblevins.org/projects/markdown-mode/) for emacs and [vim-markdown-preview](https://github.com/JamshedVesuna/vim-markdown-preview). Both open a browser window.

[Remarkable](https://remarkableapp.github.io/)

[Haroopad](http://pad.haroopress.com/)

#### Windows

[MarkdownPad](http://markdownpad.com/)



## Outlook

### Extend markdown

Pandoc is a powerful document converter and it's quite easy to extend the standard markdown. Have a look at "[Scripting with pandoc](http://pandoc.org/scripting.html)".


![](demo.svg)

This page demonstrates what can be achieved with markdown.

---

## Formatting

First off, Adam Pritchard's [Markdown Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) is very useful.

### Basics

*Italics* are created with `*italics*` or `_italics_` (text enclosed by single '\*' or single '\_'). **Boldface** is created with `**Boldface**` or `__Boldface__`  (text enclosed by double '\*' or double '_'). ~~Strikethrough~~ is written like `~~strikethrough~~` (text enclosed by double '~'). 

### Headers

```markdown
# This is an H1
## This is an H2
###### This is an H6
```

### Lists

- unordered item 1
- unordered item 2
  - subitem 3
  - subitem 4

1. ordered item 1
2. ordered item 2

```markdown
- unordered item 1
- unordered item 2
	- subitem 3
	- subitem 4
```

```markdown
1. ordered item 1
2. ordered item2
```

### Blockquotes

> This is a block quote.

```markdown
> This is a block
> quote.
```

### Links

`[Link text](URI)`

```markdown
[github repo](https://github.com/mayeranalytics/nanoPage)
```

[github repo](https://github.com/mayeranalytics/nanoPage)

### Images

Images are inserted using this syntax:

```
![alt text](path-to-file)
```

![this is an image](fibonacci.svg)

### Tables

```markdown
| column 1 | column 2 | column 3 |
|:---------|:--------:|---------:|
| Hello    | this     | is       |
| a        | table    | !        |
```

| column 1 | column 2 | column 3 |
| :------- | :------: | -------: |
| Hello    |   this   |       is |
| a        |  table   |        ! |

### Footnotes

You can create footnotes like this[^footnote].

[^footnote]: Here is the *text* of the **footnote**.

```markdown
You can create footnotes like this[^footnote].
[^footnote]: Here is the *text* of the **footnote**.
```

## Maths

Equations can be set easily by using *MathJax*. For example

$$ a^2 + b^2 = c^2 $$

can be set like this:

```
$$ a^2 + b^2 = c^2 $$
```

Inline maths is done with single-dollar quoted latex `$\forall_{i\in\mathbb N}\,\alpha_i > 0$` becomes an inline "$\forall_{i\in\mathbb N}\,\alpha_i > 0$".

Here's a more sophisticated example:

$$ \begin{bmatrix}		1 & x & 0 \\		0 & 1 & -1	\end{bmatrix}\begin{bmatrix}		1  \\		y  \\		1	\end{bmatrix}	=\begin{bmatrix}		1+xy  \\		y-1	\end{bmatrix} $$

This was produced with this $\LaTeX$ code

```latex
\begin{bmatrix} 1 & x & 0 \\ 0 & 1 & -1	\end{bmatrix}
\begin{bmatrix} 1 \\ y  \\ 1 \end{bmatrix}	
= \begin{bmatrix}		1+xy  \\	    y-1	\end{bmatrix}
```

A good quick reference can be found on [stackexchange](https://math.meta.stackexchange.com/questions/5020/mathjax-basic-tutorial-and-quick-reference).


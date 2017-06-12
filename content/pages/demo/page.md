![](demo.svg)

This page demonstrates what can be achieved with markdown.

---

## Formatting

### Headers

todo

### Lists

todo

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

Adam Pritchard's [Markdown Cheatsheet](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet) is very useful.

## Maths

Equations can be set easily. For example

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




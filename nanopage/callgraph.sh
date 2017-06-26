# install the graphmod package (stack install graphmod)
find src -name '*.hs' | xargs graphmod -q > callgraph.dot
dot -Tps callgraph.dot -o callgraph.ps

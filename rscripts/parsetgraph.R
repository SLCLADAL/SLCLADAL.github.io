## Make a graph from Tree_parse result
parse2graph <- function(ptext, leaf.color='chartreuse4', label.color='blue4',
                        title=NULL, cex.main=.9, ...) {
    stopifnot(require(NLP) && require(igraph))

    ## Replace words with unique versions
    ms <- gregexpr("[^() ]+", ptext)                                      # just ignoring spaces and brackets?
    words <- regmatches(ptext, ms)[[1]]                                   # just words
    regmatches(ptext, ms) <- list(paste0(words, seq.int(length(words))))  # add id to words

    ## Going to construct an edgelist and pass that to igraph
    ## allocate here since we know the size (number of nodes - 1) and -1 more to exclude 'TOP'
    edgelist <- matrix('', nrow=length(words)-2, ncol=2)

    ## Function to fill in edgelist in place
    edgemaker <- (function() {
        i <- 0                                       # row counter
        g <- function(node) {                        # the recursive function
            if (inherits(node, "Tree")) {            # only recurse subtrees
                if ((val <- node$value) != 'TOP1') { # skip 'TOP' node (added '1' above)
                    for (child in node$children) {
                        childval <- if(inherits(child, "Tree")) child$value else child
                        i <<- i+1
                        edgelist[i,1:2] <<- c(val, childval)
                    }
                }
                invisible(lapply(node$children, g))
            }
        }
    })()

    ## Create the edgelist from the parse tree
    edgemaker(Tree_parse(ptext))

    ## Make the graph, add options for coloring leaves separately
    g <- graph_from_edgelist(edgelist)
    vertex_attr(g, 'label.color') <- label.color  # non-leaf colors
    vertex_attr(g, 'label.color', V(g)[!degree(g, mode='out')]) <- leaf.color
    V(g)$label <- sub("\\d+", '', V(g)$name)      # remove the numbers for labels
    plot(g, layout=layout.reingold.tilford, ...)
    if (!missing(title)) title(title, cex.main=cex.main)
}
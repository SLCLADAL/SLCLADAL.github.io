netplot <- function(x = ed, edge_col = "gray80", edge_curv= .2, node_tran = .5, n_min = 2, n_max = 4, n_col = "gray10"){
  
  # setting seed so networks are reproducible
  set.seed(12345)
  
  # create node table
  va <- data.frame(c(ed$from, ed$to)) %>%
    dplyr::rename(node = 1) %>%
    dplyr::group_by(node) %>%
    dplyr::summarise(n =  n())
  
  # greate igraph object
  ig <- igraph::graph_from_data_frame(d=ed, 
                                      vertices=va, 
                                      directed = FALSE)
  
  # add labels
  tg <- tidygraph::as_tbl_graph(ig) %>% 
    tidygraph::activate(nodes)
  
  # create Fruchterman-Reingold layout graph
  tg %>%
    ggraph::ggraph(layout = "fr") +
    
    # add arcs for edges with various aesthetics
    geom_edge_arc(colour = edge_col,
                  lineend = "butt",
                  strength = edge_curv,
                  aes(edge_width = ed$s,
                      alpha = ed$s)) +
    
    # add points for nodes with size based on 'va$n'
    geom_node_point(size = va$n,
                    alpha = node_tran) +
    
    # add text labels for nodes with various aesthetics
    geom_node_text(aes(label = va$node), 
                   repel = TRUE, 
                   point.padding = unit(0.2, "lines"), 
                   size = (((va$n - min(va$n)) / (max(va$n) - min(va$n))) * (n_max - n_min) + n_min), 
                   colour = "gray10") +
    
    # adjust edge width and alpha scales
    scale_edge_width(range = c(0.5, 1)) +
    scale_edge_alpha(range = c(0.5, 1)) +
    
    # set graph background color to white
    theme_graph(background = "white") +
    
    # adjust legend position to the top
    theme(legend.position = "none", 
          # suppress legend title
          legend.title = element_blank()) +
    
    # remove edge width and alpha guides from the legend
    guides(edge_width = "none",
           edge_alpha = "none") -> np
  
  return(np)
  
}
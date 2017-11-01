### affiliation matrix
require(Matrix)
require(igraph)
require(networkD3)

mzes_proj_members <- readRDS('mzes_proj_members.RDS')

A <- spMatrix(nrow=length(unique(mzes_proj_members$member)),
              ncol=length(unique(mzes_proj_members$proj_num)),
              i = as.numeric(factor(mzes_proj_members$member)),
              j = as.numeric(factor(mzes_proj_members$proj_num)),
              x = rep(1, length(as.numeric(mzes_proj_members$member))))


row.names(A) <- levels(factor(mzes_proj_members$member))
colnames(A) <- levels(factor(mzes_proj_members$proj_num))

### The A X t(A) is the adjacency Matrix

######################

adj_mat <- tcrossprod(A)

mzes_network <- graph.adjacency(adj_mat, "undirected", weighted = TRUE, diag = FALSE)

V(mzes_network)$bet <- betweenness(mzes_network, directed = FALSE, weights = V(mzes_network)$weight)

#V(mzes_network)$constraint <- constraint(mzes_network, weights = V(mzes_network)$weight)

comm <- cluster_walktrap(mzes_network)
V(mzes_network)$comm <- membership(comm)

plot(mzes_network, vertex.size = V(mzes_network)$bet / 300, vertex.label = ifelse(V(mzes_network)$bet > 20, V(mzes_network)$name, NA), vertex.color = V(mzes_network)$comm)

####################

mzes_network_d3 <- igraph_to_networkD3(mzes_network, group = V(mzes_network)$comm)
mzes_network_d3$nodes$bet <- V(mzes_network)$bet / 3


forceNetwork(Links = mzes_network_d3$links, Nodes = mzes_network_d3$nodes,  Source = 'source', Target = 'target', NodeID = 'name', Group = 'group', Nodesize = 'bet', fontSize = 30)



open_link <- 'window.open("http://www.mzes.uni-mannheim.de/d7/en/profiles/" + d.name, "_new")'

forceNetwork(Links = mzes_network_d3$links, Nodes = mzes_network_d3$nodes,  Source = 'source', Target = 'target', NodeID = 'name', Group = 'group', Nodesize = 'bet', fontSize = 30, clickAction = open_link)

saveNetwork(forceNetwork(Links = mzes_network_d3$links, Nodes = mzes_network_d3$nodes,  Source = 'source', Target = 'target', NodeID = 'name', Group = 'group', Nodesize = 'bet', fontSize = 30, clickAction = open_link), file = "mzes_d3.html")

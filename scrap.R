require(tidyverse)
require(Matrix)
require(rvest)
require(stringr)
require(igraph)


scrap_mzes_projs <- function(url) {
  read_html(url) ->proj_a
  proj_a %>% html_nodes("div.views-field-field-project-number") %>% html_text %>% str_trim -> proj_num
  proj_a %>% html_nodes("div.research-project-title") %>% html_text %>% str_trim -> proj_title
  proj_a %>% html_nodes("div.research-project-title") %>% html_nodes('a') %>% html_attr('href') -> proj_url
  return(data_frame(proj_num, proj_title, proj_url))
}

allprojs <- map_dfr(c("http://www.mzes.uni-mannheim.de/d7/en/research-areas/a-european-societies-and-their-integration", "http://www.mzes.uni-mannheim.de/d7/en/research-areas/b-european-political-systems-and-their-integration"), scrap_mzes_projs)

scrap_mzes_proj_members <- function(proj_url) {
    print(proj_url)
    proj_html <- read_html(paste0('http://www.mzes.uni-mannheim.de', proj_url))
    proj_html %>% html_nodes('a') %>% html_attr('href') %>% grep('/profiles/', x = ., value = TRUE) %>% str_extract("/[a-zA-Z\\-]+$") %>% unique -> member
    Sys.sleep(3) ## Don't ddos mzes web server
    return(data_frame(proj_url, member))
}

allmembers <- map_dfr(allprojs$proj_url, scrap_mzes_proj_members)

allmembers %>% group_by(member) %>% tally %>% arrange(desc(n))

allmembers %>% left_join(allprojs, by="proj_url") %>% mutate(member = gsub("^/", "", member)) -> mzes_proj_members


saveRDS(mzes_proj_members, "mzes_proj_members.RDS")


### affiliation matrix
mzes_proj_members <- readRDS('mzes_proj_members.RDS')

A <- spMatrix(nrow=length(unique(mzes_proj_members$member)),
              ncol=length(unique(mzes_proj_members$proj_num)),
              i = as.numeric(factor(mzes_proj_members$member)),
              j = as.numeric(factor(mzes_proj_members$proj_num)),
              x = rep(1, length(as.numeric(mzes_proj_members$member))))


row.names(A) <- levels(factor(mzes_proj_members$member))
colnames(A) <- levels(factor(mzes_proj_members$proj_num))

### The A X t(A) is the adjacency Matrix

### intuition

example <- matrix(c(1,0,0,1,1,0,0,0,1, 1, 1, 1), nrow = 3)
colnames(example) <- c('P1', 'P2', 'P3', 'P4')
row.names(example) <- c('M1', "M2", "M3")
example

# 3*4 matrix . 4*3 matrix = 3*3 matrix

tcrossprod(example)

example %*% t(example)

######################

adj_mat <- tcrossprod(A)

mzes_network <- graph.adjacency(adj_mat, "undirected", weighted = TRUE, diag = FALSE)

V(mzes_network)$bet <- betweenness(mzes_network, directed = FALSE, weights = V(mzes_network)$weight)

#V(mzes_network)$constraint <- constraint(mzes_network, weights = V(mzes_network)$weight)


V(mzes_network)$comm <- membership(cluster_walktrap(mzes_network))

plot(mzes_network, vertex.size = V(mzes_network)$bet / 300, vertex.label = ifelse(V(mzes_network)$bet > 10, V(mzes_network)$name, NA), vertex.color = V(mzes_network)$comm)

####################

#install.packages('networkD3')

require(networkD3)

mzes_network_d3 <- igraph_to_networkD3(mzes_network, group = V(mzes_network)$comm)
mzes_network_d3$nodes$bet <- V(mzes_network)$bet / 3


forceNetwork(Links = mzes_network_d3$links, Nodes = mzes_network_d3$nodes,  Source = 'source', Target = 'target', NodeID = 'name', Group = 'group', Nodesize = 'bet', fontSize = 30)


open_link <- 'window.open("http://www.mzes.uni-mannheim.de/d7/en/profiles/" + d.name, "_new")'

forceNetwork(Links = mzes_network_d3$links, Nodes = mzes_network_d3$nodes,  Source = 'source', Target = 'target', NodeID = 'name', Group = 'group', Nodesize = 'bet', fontSize = 30, clickAction = open_link)

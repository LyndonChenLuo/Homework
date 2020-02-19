
# load packages
library(sna)
library(network)

ceosClubs <- read.table(file='./Lab3CEOs.csv', 
                        header=TRUE, 
                        row.names=1,  # need to set the row names, because the first column does not contain relationships
                        stringsAsFactors=FALSE, 
                        nrows=27, 
                        sep=',')
# take a look at the first six rows
head(ceosClubs)

# num of rows and columns of this data frame 
dim(ceosClubs)
# result shows there are 26 CEOs (actors) and 15 clubs (events) in this data

# data type of each column
str(ceosClubs)

# turn the data frame into a network object
net <- network(ceosClubs, 
               directed=TRUE,  # logical, should edges be interpreted as directed?
               loops=FALSE,  # logical, should loops be allowed?
               bipartite=TRUE,  # the data contains both actors and events, so the `directed=TRUE` is not valid
               matrix.type='adjacency')  # data is composed of 0 and 1
# examine the new network
# bipartite = 26 means the first 26 nodes are actors and the remaining 15 nodes are events
summary(net)

# display the very rough network vis
plot(net)

# project the affiliation network onto one mode
# need to multiply the matrices, first, turn the network object into matrix
net.matrix <- as.matrix.network(net)
# multiply the matrix by its transposed
ceo.net <- net.matrix %*% t(net.matrix)
# take a look at the matrix
ceo.net

# dichotomize the valued network of shared clubs for the CEOs
# set the cutoff point threshold as 2
ceo.net[,] <- event2dichot(ceo.net, 
                           method='absolute', 
                           thresh=2)
# turn the dichotomized matrix into a network
ceo.net <- as.network.matrix(ceo.net, 
                             directed=FALSE)
# examine the network
summary(ceo.net)

# take a look at the network data
ceo.net[,]

# this is a symmetric network, so mode = "graph"
round(gden(ceo.net, mode="graph"), digits=4)

# this is a symmetric network, so gmode = "graph"
# calculate degree centrality of each node
degree(ceo.net, gmode="graph")

# calculate betweenness centrality of each node
betweenness(ceo.net, gmode="graph")

# calculate network's degree centralization
round(centralization(ceo.net, FUN=degree, mode="graph"), digits=4)

# calculate network's betweenness centralization
round(centralization(ceo.net, FUN=betweenness, mode="graph"), digits=4)

# calculate network's closeness centralization
round(centralization(ceo.net, FUN=closeness, mode="graph"), digits=4)

# transitivity is a triadic, algebraic structural constraint
# measure = "weak" means the fraction of potentially intransitive triads obeying the weak condition will be returned
round(gtrans(ceo.net, 
       mode="graph",  # this network is a undirected graph, it means undirected triads are sought
       measure="weak"), digits=4)

# measure = "weakcensus" returns the number of weakly transitive triads
gtrans(ceo.net, 
       mode="graph", 
       measure="weakcensus")

# measure = "strong" returns proportion of triads where a->b->c <=> a->c
round(gtrans(ceo.net, 
       mode="graph", 
       measure="strong"), digits=4)

# measure = "strongcensus" returns the number of strongly transitive triads
gtrans(ceo.net, 
       mode="graph", 
       measure="strongcensus")

# a rough network helps to understand the cliques
plot(ceo.net, 
     displaylabels=TRUE)

# clique.comembership = "sum" returns the total number of shared cliques for each pair of nodes
ceo.net.cliques <- clique.census(ceo.net, 
                                 mode="graph", 
                                 clique.comembership="sum")
# returns an object contains the result of the clique census
ceo.net.cliques

# conclusions from the results
# 1. There are 10 + 3 + 5 + 1 + 1 = 20 cliques in this network

# 2. Among the cliques, 10 cliques have only 1 node (isolated nodes in the above figure)
# ...3 cliques have 2 nodes
# ...5 cliques have 3 nodes
# ...1 clique has 5 nodes and 1 clique has 6 nodes

# 3. the second element of this result is the clique co-membership matrix, which contains the clique comembership scores
# ...also, it can be accessd by `ceo.net.cliques$clique.comemb`

# 4. the third element of this result is a list of all found cliques
# ...also, it can be accessed by `ceo.net.cliques$cliques`

# return the clique comembership scores
ceo.net.cliques$clique.comemb

# return the list of all cliques
ceo.net.cliques$cliques

# structural similarity is a part of structural equivalence, `method = correlation` means product-moment correlation
round(sedist(ceo.net, 
       mode="graph", 
       method="correlation"), digits=4)

# use the help function to find more parameters
?sna::gplot

list.network.attributes(ceo.net)

set.vertex.attribute(ceo.net, "degree.centrality", degree(ceo.net, gmode="graph"))
summary(ceo.net)

get.vertex.attribute(ceo.net, "degree.centrality")

node_colors <- rep("", get.network.attribute(ceo.net, "n"))
for (i in 1: get.network.attribute(ceo.net, "n")){
    if (get.vertex.attribute(ceo.net, "degree.centrality")[i] < 2.538){  # use the mean value of degree centrality as a threshold
        node_colors[i] <- "lightblue"
    } else {
        node_colors[i] <- "maroon"
    }
}
print(node_colors)

node_sizes <- rep(1, get.network.attribute(ceo.net, "n"))
for (i in 1: get.network.attribute(ceo.net, "n")){
    if (get.vertex.attribute(ceo.net, "degree.centrality")[i] == 0){  # adjustments for isolated nodes
        node_sizes[i] <- 0.2
    } else {
        node_sizes[i] <- get.vertex.attribute(ceo.net, "degree.centrality")[i] / 4  # avoid too large nodes
    }
}
print(node_sizes)

# draw the network
gplot(ceo.net, 
      gmode="graph", 
      vertex.cex=node_sizes, 
      vertex.sides=18, 
      usearrows=FALSE,  # arrows are not needed for indicating edges
      vertex.col=node_colors)

# save the result as PDF
# start a graphic device
pdf("./vis.pdf")
# network goes to PDF
gplot(ceo.net, gmode="graph", vertex.cex=node_sizes, vertex.sides=18, usearrows=FALSE, vertex.col=node_colors)
dev.off()

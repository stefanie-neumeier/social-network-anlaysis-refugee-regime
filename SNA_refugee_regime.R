
####                                                                                                     #####
#### This repo provides code for turning an edgelist into a network object, calculate various node       #####
#### and structure level measures, and visualize the network based on different communities and clusters.#####
###3                                                                                                     #####


## Upload data: First we are going to load the edgelist (each node (actor) and 
## its connections (partners) as well as the meta dataset, which records some characteristics of 
## these actors (geographic region, name, etc) 


setwd("~/Documents/Documents_08042020/Fall 2019_3rd year/CSS/project")

library(readxl)
edges <- read_excel("edgelist_full_assigned.xlsx", 
                    sheet = "edgelist_done")

nodes <- read_excel("edgelist_full_assigned.xlsx", 
                    sheet = "unique IDs ")


# Now we use igraph and turn our edgelist into a graph object. 
# We use the nodes dataset to let igraph know what our nodes are.
library(quanteda)
library(igraph)

g <- graph_from_data_frame(d=edges, vertices = nodes, directed=F)
g


##  Now we have an igraph object. This is an undirected network. We can see this
## in the first line of the output: UN-- stands for undirected. We have 861 actors
## in the network and 1263 edges (ties). We also have attributes of our actors; 
## those were imported as we indicated our nodes dataset (vertices = nodes) 
## when we create the graph object. Some of the attributes for our actors are: country, city_hq, region. 


## Next, to get a better feel of what our network looks like: Let's graph the Network 

l <- layout_with_fr(g) # create a layout for the graph 
l <- norm_coords(l, ymin=-0.5, ymax=0.5, xmin=-0.6, xmax=0.6)  # specifiy the coordinates 

set.seed(1)
plot(g, vertex.size=4, vertex.label.cex=0.25, vertex.label.font = 1, layout= l*1.7, rescale = FALSE,
     vertex.label.color = "black")


## This is an interesting network, there seems to be a core as well as quite some clustering.
## We also have a few isolated organizations. Let's dig deeper and look at centrality measures. 



########## EDA: Different Centrality Measures ##########

## Central actors: Degree, Betweeness centrality 

## DEGREE 

tail(sort(degree(g)))v
### most central: 11102 UNHCR, 10890 IOM, UnWRA 11104, IRC 10895,  Norwegian Refugee Council 10960, ICMC 


V(g)$degree <- degree(g, normalized = T) # attach to graph object and nodes 

set.seed(1)
plot(g,
     vertex.color = "yellow",
     vertex.size = V(g)$degree*25,
     vertex.label.cex = 0.3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5,
     layout= l*1.9, rescale = FALSE)


## Mapping the network based on degree centrality, we can clearly see two leaders 
## (or most central actors based on degree centrality): the United Refugee Agency (UNHCR) 
## and the International Organization for Migration (IOM). The UNHCR is by far the most central 
## actor in the networ (degree centrality). This is sensible as it is THE most important refugee organization. 

## Let's try a different type of centrality: betweenness. Betweenness differs
## from degree centrality as it focuses on the shortest paths in the network rather
## than on count of connections. Put differently, actors or nodes with high betweenness 
## centrality often hold "gatekeeper" positions as they connect to many different (often remote actors). 
## So information and resources flow through them. 


### BETWEEN ##### 

V(g)$between <- betweenness(g, normalized=T)
tail(sort(betweenness(g)), normalized = T)
## UNHCR: highest between, IOM second highest,  UNRWA 3rd, IRC 4th, DPI 5th, ICMC 6th 


## Let's plot the network again based on betweenness: 

set.seed(1)
plot(g,
     vertex.color = "coral",
     vertex.size = V(g)$between*15,
     vertex.label.cex = 0.3,
     vertex.label.font = 1.0,
     vertex.label.color = "black",
     edge.width = 0.5,
     layout= l*1.9, rescale = FALSE)

## Once again, the UNHCR and IOM remain the most central in the network. They truly 
## seem to hold gatekeeper positions. 

## Having looked at some node-specific centrality measures, it is also possible
## to explore the overall structure of the network more. For example, what is the 
## overall density, transitivity, and the mean distance of the network? These 
## structural measures give insights into what type of network we are dealing with.
## Is it a sparse network? How long does it take, on average, to reach other nodes 
## in the network? Are there many closed triangles (meaning if A knows B and C,
## it is likely that B and C also know each other)?


centr_degree(g)$centralization  ### somewhat high: 0.40 --> this speaks confirms that the network
# is rather decentralized and hierarchical, with only a few nodes/actors being most important 


edge_density(g)  ##3 0.003 --> also rather low, this is expected in a large network like this 
transitivity(g) ### 0.012 --> low (there are not many closed triangles)
mean_distance(g) ### 3.19 --> nodes are on average 3.19 steps away from one another 


mean(degree(g)) ### average is 2.9 (on average, actors have about 3 connections) 
head(table(degree(g)))


######## Community Detection and Clustering #####

## It is especially interesting to explore the  network based on communities and clusters.
## The connections that actors make can tell us something about the programs, their focus,etc.
## It is also indicative how many different communities there are in a network. 
## In some networks, there are very few as actors are well connected; in big networks 
## there may be many communities and clusters around different issue areas, actors, geographic regions etc. 


## Edge-betweenness is one type of community detection: The idea is that there are
## actors that function as connectors; as we,step by step, remove these high-betweeness actors,
## we end up with a hierarchical graph. So the algorithm finds the nodes with the 
## highest betweenness score and removes them --> this may result in the graph splitting
## into different graphs --> that is the first partition. Then the algorithm keeps removing
## the nodes with high edge-betweeness again resulting in the splitting/rearranging of the network 

# first create edge_betweenness and attach to the meta dataset (nodes)

edge_between <- cluster_edge_betweenness(g)
nodes$edge_between <- membership(edge_between)
edge_between # here are all the different groups; there are overall 36 clusters 


## We can graph the network based on edge betweenness

library(pals) 

c5 <- polychrome(36)[nodes$edge_between] # we have 36 groups so we need 36 colors 
vertex.color = c5 # assign this to vertex color 

# plot based on edge betweenness groups 
set.seed(1)
plot(g, vertex.color= c5, vertex.label.cex = 0.25, vertex.size = 3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5, layout= l*1.8, rescale = FALSE)


## As we can see in this plot, there are a few distinct clusters, there is a large
## red and a large grey cluster. The grey cluster is the UNHCR cluster and the red 
## cluster is the IOM cluster. The IOM and UNHCR are two of the biggest migration/refugee 
## organizations so this makes sense that they have many connections and form distinct
## clusters. Let's look into this a bit more: 

# The two biggest clusters; here are some of the nodes in it 
nodes$org_id[nodes$edge_between==1] ### this is the UNHCR cluster
nodes$org_id[nodes$edge_between==3] ### IOM cluster

# Here are examples of what type of organizations are in the biggest clusters in the network 
set.seed(11)
sample(nodes$organization[nodes$edge_between==1], 5, replace = FALSE, prob = NULL) ## biggest cluster 
set.seed(15)
sample(nodes$organization[nodes$edge_between==3], 5, replace = FALSE, prob = NULL) ### 2nd biggest cluster
set.seed(20)
sample(nodes$organization[nodes$edge_between==2], 5, replace = FALSE, prob = NULL) ### 3rd biggest cluster

## We can also look at who is in the smaller clusters just to get a feel


# sample of smaller clusters: who is in cluster 36 
set.seed(18)
sample(nodes$organization[nodes$edge_between==36], 5, replace = FALSE, prob = NULL) ## Christian-faith based 
# who is in cluster 33? 
sample(nodes$organization[nodes$edge_between==33], 5, replace = FALSE, prob = NULL)## Jewish-faith based
## Who is in cluster 23
sample(nodes$organization[nodes$edge_between==23], 3, replace = FALSE, prob = NULL) ### African/Carribean specific 


## As we can see both mapping and then a more in-depth interrogation of the individual 
## clusters gives insights into clustering dynamics.

## As a next step, we are going to look at the core-periphery structure. K-core decomposition
## allows us to identify the core and the periphery of the network. The k-core of a graph is 
## the maximal subgraph in which every vertex has at least degree k. Core nodes are well connected,
## and peripheral nodes are sparsely connected.

# coreness analysis 
summary(coreness(g)) # the min is 1 and the max is 8 

nodes$core <- coreness(g)  # create column in the dataset 

c6 <- brewer.blues(8)[nodes$core] # we have 8 groups so we need 8 colors 
vertex.color = c6 # assign this to vertex color 

# plot based on coreness 
set.seed(1)
plot(g, vertex.color= c6, vertex.label.cex = 0.25, vertex.size = 3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5, layout= l*1.75, rescale = FALSE)
legend("bottomleft", 
       legend=c("Periphery", "Core"),
       col=c("#DEEBF7", "#08519C"), pch=c(19), cex = 0.7, x.intersp= 0.5, y.intersp = 0.9,  bty='n', inset=c(0, 0))

## As we can see here, "coreness" increases with the intensity of the blue. Unsurprisingly, 
## the most core actors are at the core of the network. However, the number of core actors 
## is rather limited. Again the UNHCR and the IOM appear as a dark blue. 



########### We can graph the network based on geographic region. ###########

## First, we created the regions and attache them to the meta data  

# first we create broad geographic regions 
nodes$region_broad <- ifelse(nodes$region== "Central Europe" | nodes$region== "East Europe", "Europe", 
                             ifelse(nodes$region== "South America" | nodes$region== "Central America" | nodes$region== "Caribbean", "South America & Caribbean",
                                    ifelse(nodes$region== "South Asia" | nodes$region== "East Asia", "Asia",
                                           ifelse(nodes$region== "Central Africa" | nodes$region== "West Africa" | nodes$region== "East Africa" | nodes$region == "Southern Africa", "Africa",
                                                  ifelse(nodes$region== "Middle East" | nodes$region== "North Africa", "North Africa & ME",
                                                         ifelse(nodes$region== "North America", "North America", "none")
                                                  )))))



V(g)$color <- ifelse(nodes$region_broad== "Europe", "cyan", 
                     ifelse(nodes$region_broad== "South America & Caribbean", "green", 
                            ifelse(nodes$region_broad == "Africa", "red",
                                   ifelse(nodes$region_broad == "Asia", "blue",
                                          ifelse(nodes$region_broad == "North America", "yellow",
                                                 ifelse(nodes$region_broad == "North Africa & ME", "coral", "black")
                                          )))))


## Now we're ready to graph based on geographic regions 

set.seed(1)
plot(g,
     vertex.size = 3,
     vertex.color = V(g)$color,
     vertex.label.cex = 0.25,
     vertex.label.font = 1.0,
     vertex.label.color = "black",
     edge.width = 0.5,
     layout= l*1.7, rescale = FALSE)
legend("bottomleft", 
       legend=c("Europe", "North America", "South America & Caribbean", "Africa", "Asia", "North Africa & ME"),
       col=c("cyan","yellow","green","red", "blue", "coral"), pch=c(19), cex = 0.7, x.intersp= 0.5, y.intersp = 0.9,  bty='n', inset=c(0, 0))


## We can see that the network appears to be pretty heterogenous: a lot of different 
## actors from different regions partnering up together. However, the graph also shows 
## that European and North American actors are dominating the network.

####### There are more ways of exploring the network depending on what type of meta data is ########
####### available for the nodes. As a next step, we could do some very simple network ########
####### statisical models to see what influences tie formation in the network. ########















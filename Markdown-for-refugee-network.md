Social Network Analysis: Refugee Regime
================
Stefanie Neumeier
5/6/2021

### This repo provides code for turning an edgelist into a network object, calculate various node and structure level measures, and visualize the network based on different communities and clusters.

``` r
knitr::opts_knit$set(root.dir = "~/Documents/Documents_08042020/Fall 2019_3rd year/CSS/project")
```

``` r
getwd()
```

    ## [1] "/Users/stefanien/Documents/Documents_08042020/Fall 2019_3rd year/CSS/project"

**Upload data: First we are going to load the edgelist (each node
(actor) and its connections (partners) as well as the meta dataset,
which records some characteristics of these actors (geographic region,
name, etc) **

``` r
library(readxl)

edges <- read_excel("edgelist_full_assigned.xlsx", 
    sheet = "edgelist_done")
    
nodes <- read_excel("edgelist_full_assigned.xlsx", 
    sheet = "unique IDs ")
```

-----

Now we use igraph and turn our edgelist into a graph object. We use the
nodes dataset to let igraph know what our nodes are.

``` r
library(quanteda)
```

    ## Package version: 2.1.1

    ## Parallel computing: 2 of 4 threads used.

    ## See https://quanteda.io for tutorials and examples.

    ## 
    ## Attaching package: 'quanteda'

    ## The following object is masked from 'package:utils':
    ## 
    ##     View

``` r
library(igraph)
```

    ## 
    ## Attaching package: 'igraph'

    ## The following object is masked from 'package:quanteda':
    ## 
    ##     as.igraph

    ## The following objects are masked from 'package:stats':
    ## 
    ##     decompose, spectrum

    ## The following object is masked from 'package:base':
    ## 
    ##     union

``` r
g <- graph_from_data_frame(d=edges, vertices = nodes, directed=F)

g
```

    ## IGRAPH e607bcf UN-- 861 1263 -- 
    ## + attr: name (v/c), organization (v/c), refugee_org (v/n), country
    ## | (v/c), city_hq (v/c), region (v/c)
    ## + edges from e607bcf (vertex names):
    ##  [1] 10644--10322 10644--10356 10644--10469 10659--10012 10659--10718
    ##  [6] 10660--10686 10660--10180 10660--10934 10660--10988 10660--11007
    ## [11] 10660--11102 10320--10661 10661--10358 10661--10081 10661--10526
    ## [16] 10661--11102 10661--10628 10666--11013 10887--10672 10554--10672
    ## [21] 10672--10276 10088--10674 10674--10426 10674--10437 10578--10674
    ## [26] 11104--10674 10070--10675 10675--10080 10102--10675 10675--10105
    ## [31] 10003--10675 10356--10675 10675--10426 10675--10437 10675--10504
    ## + ... omitted several edges

Now we have an igraph object. This is an undirected network. We can see
this in the first line of the output: UN– stands for undirected. We have
861 actors in the network and 1263 edges (ties). We also have attributes
of our actors; those were imported as we indicated our nodes dataset
(vertices = nodes) when we create the graph object. Some of the
attributes for our actors are: country, city\_hq, region.

**Next, to get a better feel of what our network looks like: Let’s Graph
the Network**

``` r
l <- layout_with_fr(g)
l <- norm_coords(l, ymin=-0.5, ymax=0.5, xmin=-0.6, xmax=0.6)


set.seed(1)
plot(g, vertex.size=4, vertex.label.cex=0.25, vertex.label.font = 1, layout= l*1.7, rescale = FALSE,
     vertex.label.color = "black")
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

This is an interesting network, there seems to be a core as well as
quite some clustering. WE also have a few isolated organizationns. Let’s
dig deeper and look at centrality measures.

-----

**EDA: Different Centrality Measures**

``` r
#### Central actors: Degree, Betweeness centrality 

### DEGREE 

tail(sort(degree(g)))
```

    ## 10192 10960 10895 11104 10890 11102 
    ##    41    43    57    59   210   352

``` r
### most central: 11102 UNHCR, 10890 IOM, UnWRA 11104, IRC 10895,  Norwegian Refugee Council 10960, ICMC 

V(g)$degree <- degree(g, normalized = T)

set.seed(1)
plot(g,
     vertex.color = "yellow",
     vertex.size = V(g)$degree*25,
     vertex.label.cex = 0.3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5,
     layout= l*1.9, rescale = FALSE)
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Mapping the network based on degree centrality, we can clarly see two
leaders (or most central actors based on degree centrality): the United
Refugee Agency (UNHCR) and the International Organization for Migration
(IOM). The UNHCR is by far the most central actor in the networ (degree
centrality). This is sensible as it is THE most important refugee
organization.**

-----

**Let’s try a different type of centrality: betweenness. Betweenness
differs from degree centrality as it focuses on the shortest paths in
the network rather than on count of connections. Put differently, actors
or nodes with high betweenness centrality often hold “gatekeeper”
positions as they connect to many different (often remote actors). So
information and resources flow through them.**

``` r
### BETWEEN ##### 

V(g)$between <- betweenness(g, normalized=T)
tail(sort(betweenness(g)), normalized = T)
```

    ##     10192     10049     10895     11104     10890     11102 
    ##  23120.79  23824.09  28668.23  31601.78 122736.66 239638.39

``` r
### UNHCR: highest between, IOM second highest,  UNRWA 3rd, IRC 4th, DPI 5th, ICMC 6th 
```

Let’s plot the network to see if the same actors are central:

``` r
set.seed(1)
plot(g,
     vertex.color = "coral",
     vertex.size = V(g)$between*15,
     vertex.label.cex = 0.3,
     vertex.label.font = 1.0,
     vertex.label.color = "black",
     edge.width = 0.5,
     layout= l*1.9, rescale = FALSE)
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Once again, the UNHCR and IOM remain the most central in the network.
They truly seem to hold gatekeeper positions.

-----

**Having looked at some node-specific centrality measures, it is also
possible to explore the overall structure of the network more. For
example, what is the overall density, transitivity, and the mean
distance of the network? These strucural measure give insights into what
type of network we are dealing with. Is it a sparse network? How long
does it take, on average, to reach other nodes in the network? Are there
many closed triangles (meaning if A knows B and C, it is likely that B
and C also know each other)?**

``` r
centr_degree(g)$centralization  ### somewhat high: 0.40 --> this speaks confirms that the network
```

    ## [1] 0.4058909

``` r
# is rather decentralized and hierarchical, with only a few nodes/actors being most important 


edge_density(g)  ##3 0.003
```

    ## [1] 0.003411393

``` r
transitivity(g) ### 0.012 --> low
```

    ## [1] 0.01195073

``` r
mean_distance(g) ### 3.19 --> nodes are on average 3.19 steps away from one another 
```

    ## [1] 3.190772

``` r
mean(degree(g)) ### average is 2.9 (on average, actors have about 3 connections) 
```

    ## [1] 2.933798

``` r
head(table(degree(g)))
```

    ## 
    ##   1   2   3   4   5   6 
    ## 652  72  44  21  12  12

-----

**Community Detection and Clustering: It is especially interesting to
explore the network based on communities and clusters. The connections
that actors make can tell us something about the programs, their
focus,etc. It is also indicative how many different communities there
are in a network. In some networks, there are very few as actors are
well connected; in big networks there may be many communities and
clusters around different issue areas, actors, geographic regions etc.**

Edge-betweenness is one type of community detection: The idea is that
there are actors that function as connections, as we step by step remove
these high-betweeness actors, we end up with a hierarchical graph. So
the algorithm finds the nodes with the highest betweenness score and
removes them –\> this may result in the graph splitting into different
graphs –\> that is the first partition. Then the algorithm keeps
removing the nodes with high edge-betweeness again resulting in the
splitting/rearranging of the network

``` r
# first create edge_betweenness and attach to the meta dataset (nodes)
edge_between <- cluster_edge_betweenness(g)
nodes$edge_between <- membership(edge_between)
edge_between # here are all the different groups; there are overall 36 clusters 
```

    ## IGRAPH clustering edge betweenness, groups: 36, mod: 0.6
    ## + groups:
    ##   $`1`
    ##     [1] "11074" "10847" "10820" "10679" "10700" "10476" "10938" "10991" "10998"
    ##    [10] "11045" "10641" "10650" "10701" "10930" "11006" "11035" "11106" "10753"
    ##    [19] "10811" "10977" "11053" "10750" "10740" "10793" "10883" "10639" "10741"
    ##    [28] "10840" "10695" "10687" "10855" "11125" "10713" "10738" "10909" "10662"
    ##    [37] "10768" "10742" "10759" "10689" "10751" "10755" "10819" "10688" "10769"
    ##    [46] "10974" "10770" "10818" "10992" "10677" "11034" "10782" "10767" "10846"
    ##    [55] "10951" "10786" "10656" "10647" "10776" "11038" "11116" "10822" "10937"
    ##    [64] "10983" "10804" "10805" "10646" "10748" "10801" "11108" "10979" "11090"
    ##    [73] "10774" "10925" "10670" "11051" "10722" "10982" "10783" "10971" "10912"
    ##   + ... omitted several groups/vertices

**We can graph the network based on edge betweenness**

``` r
library(pals) 

c5 <- polychrome(36)[nodes$edge_between] # we have 36 groups so we need 36 colors 
vertex.color = c5 # assign this to vertex color 

# plot based on edge betweenness groups 
set.seed(1)
plot(g, vertex.color= c5, vertex.label.cex = 0.25, vertex.size = 3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5, layout= l*1.8, rescale = FALSE)
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

As we can see in this plot, there are a few distinct clusters, there is
a large red and a large grey cluster. The grey cluster is the UNHCR
cluster and the red cluster is the IOM cluster. The IOM and UNHCR are
two of the biggest migration/refugee organizations so this makes sense
that they have many connections and form distinct clusters. Let’s look
into this a bit more:

``` r
# The two biggest clusters; here are some of the nodes in it 
nodes$org_id[nodes$edge_between==1] ### this is the UNHCR cluster
```

    ##   [1] 11074 10847 10820 10679 10700 10476 10938 10991 10998 11045 10641 10650
    ##  [13] 10701 10930 11006 11035 11106 10753 10811 10977 11053 10750 10740 10793
    ##  [25] 10883 10639 10741 10840 10695 10687 10855 11125 10713 10738 10909 10662
    ##  [37] 10768 10742 10759 10689 10751 10755 10819 10688 10769 10974 10770 10818
    ##  [49] 10992 10677 11034 10782 10767 10846 10951 10786 10656 10647 10776 11038
    ##  [61] 11116 10822 10937 10983 10804 10805 10646 10748 10801 11108 10979 11090
    ##  [73] 10774 10925 10670 11051 10722 10982 10783 10971 10912 10690 10899 11001
    ##  [85] 10746 10839 10756 10900 10758 10762 10897 10747 10814 10984 10852 10905
    ##  [97] 10735 10911 10253 11052 10645 11026 10913 11003 10815 10837 10898 11046
    ## [109] 11027 11032 10667 10671 10932 11002 10665 10919 10924 10806 11060 10777
    ## [121] 11057 10965 10694 10784 11047 11105 10914 10967 10970 10692 10807 10939
    ## [133] 10947 10872 10869 10945 10779 10948 10949 10950 11005 11017 11049 10715
    ## [145] 10966 10648 10693 10737 10957 10754 11031 10813 10752 10994 10942 10997
    ## [157] 11030 10808 10825 10963 10655 10649 10916 11132 10716 11124 10830 11115
    ## [169] 10848 10684 10969 10953 10668 10744 11066 11067 11085 10322 10831 10863
    ## [181] 10871 10910 10990 11029 11042 11058 10972 10279 11102 11127 10708 10140
    ## [193] 10826 10827 10828 10103 10833 11071 11072 11073 11076 10683 10682 10732
    ## [205] 10809 10678 11010 11061 10691 10812 10836 10968 10657 10961 11011 10142
    ## [217] 10792 10173 10902 10989 11033 10318 10723 10844 10845 10850 10766 10927
    ## [229] 10952 10929 10699 10734 10865 10868 10479 10931 10933 10940 10975 11008
    ## [241] 11025 11036 11044 11075 11081 11082 11110 11119 11121 10652 10669 10707
    ## [253] 10733 10878 10920 10985 10709 10838 10849 10854 10880 10778 10800 10851
    ## [265] 11050

``` r
nodes$org_id[nodes$edge_between==3] ### IOM cluster
```

    ##   [1] 10629 10704 10028 10166 11014 10031 10161 10078 10193 10252 10275 10288
    ##  [13] 10298 10199 10362 10001 10328 10005 10146 10037 10165 10886 10213 10496
    ##  [25] 10255 10274 10280 10314 10698 10531 10553 10568 10206 10066 10266 10295
    ##  [37] 10145 10452 10195 10034 10158 10488 10312 10234 10654 10294 10160 10155
    ##  [49] 10188 10214 10249 10268 10272 10384 10100 10107 10304 10311 10405 10133
    ##  [61] 10051 10350 10084 10202 10904 10219 10493 10002 10335 10457 10220 10221
    ##  [73] 10243 10269 10617 10151 10169 10150 10935 10540 10277 10131 10087 10189
    ##  [85] 10470 10229 10281 10198 10232 10149 10215 10254 10542 10184 10124 10379
    ##  [97] 10096 10026 10041 10364 10063 10168 10174 10178 10077 10444 10200 10208
    ## [109] 10890 10212 10245 10260 10585 10289 10290 10114 10302 10308 10237 10177
    ## [121] 10205 10135 10014 10127 10144 10171 10424 10182 10185 10190 10201 10251
    ## [133] 10282 10597 10574 10112 10612 10306 10183 10259 10231 10022 10132

``` r
# Here are examples of what type of organizations are in the biggest clusters in the network 
set.seed(11)
sample(nodes$organization[nodes$edge_between==1], 5, replace = FALSE, prob = NULL) ## biggest cluster 
```

    ## [1] "Advocates for Public Interest Law"                  
    ## [2] "Centre for International Governance Innovation"     
    ## [3] "RETInternational"                                   
    ## [4] "Canterbury Refugee Resettlement and Resource Centre"
    ## [5] "MENA Civil Society Network for Displacement"

``` r
set.seed(15)
sample(nodes$organization[nodes$edge_between==3], 5, replace = FALSE, prob = NULL) ### 2nd biggest cluster
```

    ## [1] "Central American Commission for Migration (OCAM)"                  
    ## [2] "International Centre for Migration, Health and Development (ICMHD)"
    ## [3] "Inter-American Institute for Cooperation on Agriculture (IICA)"    
    ## [4] "Refugee Council of Australia (RCOA)"                               
    ## [5] "International Federation of Red Cross and Red Crescent Societies"

``` r
set.seed(20)
sample(nodes$organization[nodes$edge_between==2], 5, replace = FALSE, prob = NULL) ### 3rd biggest cluster
```

    ## [1] "Business Humanitarian Forum (BHF)"                   
    ## [2] "Disaster Affected Communities Network (CDAC Network)"
    ## [3] "UN Coordinating Action in Small Arms (CASA)"         
    ## [4] "Act for Peace"                                       
    ## [5] "Refugees international (RI)"

**We can also look at who is in the smaller clusters just to get a
feel**

``` r
### sample of smaller clusters: who is in cluster 36 

set.seed(18)
sample(nodes$organization[nodes$edge_between==36], 5, replace = FALSE, prob = NULL) ## Christian-faith based 
```

    ## [1] "Anglican Communion"                  
    ## [2] "Lutheran World Federation (LWF)"     
    ## [3] "Church World Service (CWS)"          
    ## [4] "WorldReliefRefugeeServices\t(WRRS)"  
    ## [5] "Episcopal Migration Ministries (EMM)"

``` r
# who is in cluster 33? 
sample(nodes$organization[nodes$edge_between==33], 5, replace = FALSE, prob = NULL)## Jewish-faith based
```

    ## [1] "AfricanRefugeeDevelopmentCenter\t(ARDC)"     
    ## [2] "HIAS"                                        
    ## [3] "ASSAF"                                       
    ## [4] "Mesila"                                      
    ## [5] "Refugee Rights Clinic at Tel Aviv University"

``` r
## Who is in cluster 23
sample(nodes$organization[nodes$edge_between==23], 3, replace = FALSE, prob = NULL) ### African/Carribean specific 
```

    ## [1] "AfricanBlackDiasporaGlobalNetwork\t(ABDGN)"      
    ## [2] "Caribbean Vulnerable Communities Coalition (CVC)"
    ## [3] "African Health Policy Network (AHPN)"

**As we can see both mapping and then a more in-depth interrogation of
the individual clusters gives insights into clustering dynamics.**

-----

**As a next step, we are going to look at the core-periphery structure.
K-core decomposition allows us to identify the core and the periphery of
the network. The k-core of a graph is the maximal subgraph in which
every vertex has at least degree k. Core nodes are well connected, and
peripheral nodes are sparsely connected.**

``` r
#### coreness analysis 
summary(coreness(g)) # the min is 1 and the max is 8 
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   1.000   1.000   1.539   1.000   8.000

``` r
nodes$core <- coreness(g)  # create column in the dataset 

c6 <- brewer.blues(8)[nodes$core] # we have 8 groups so we need 8 colors 
vertex.color = c6 # assign this to vertex color 
```

now graph:

``` r
# plot based on coreness 
set.seed(1)
plot(g, vertex.color= c6, vertex.label.cex = 0.25, vertex.size = 3,
     vertex.label.font = 1,
     vertex.label.color = "black",
     edge.width = 0.5, layout= l*1.75, rescale = FALSE)
legend("bottomleft", 
       legend=c("Periphery", "Core"),
       col=c("#DEEBF7", "#08519C"), pch=c(19), cex = 0.7, x.intersp= 0.5, y.intersp = 0.9,  bty='n', inset=c(0, 0))
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-18-1.png)<!-- -->

**As we can see here, “coreness” increases with the intensity of the
blue. Unsurprisingly, the most core actors are at the core of the
network. However, the number of core actors is rather limited. Again the
UNHCR and the IOM appear as a dark blue. **

-----

**We can graph the network based on geographic region. First, we created
the regions and attache them to the meta data **

``` r
# first we create broad geographic regions 
nodes$region_broad <- ifelse(nodes$region== "Central Europe" | nodes$region== "East Europe", "Europe", 
                             ifelse(nodes$region== "South America" | nodes$region== "Central America" |
                                         nodes$region== "Caribbean", "South America & Caribbean",
                                    ifelse(nodes$region== "South Asia" | nodes$region== "East Asia", "Asia",
                                           ifelse(nodes$region== "Central Africa" | nodes$region== "West Africa" |
                                                      nodes$region== "East Africa" | nodes$region == "Southern
                                                                                    Africa", "Africa",
                                                  ifelse(nodes$region== "Middle East" | nodes$region== "North
                                                                                    Africa", "North Africa & ME",
                                                         ifelse(nodes$region== "North America", "North America",
                                                                                                    "none")
                                                  )))))

# Then we'll assign colors based on those regions 
V(g)$color <- ifelse(nodes$region_broad== "Europe", "cyan", 
                     ifelse(nodes$region_broad== "South America & Caribbean", "green", 
                            ifelse(nodes$region_broad == "Africa", "red",
                                   ifelse(nodes$region_broad == "Asia", "blue",
                                          ifelse(nodes$region_broad == "North America", "yellow",
                                                 ifelse(nodes$region_broad == "North Africa & ME", "coral", "black")
                                                 )))))
```

**Now we’re ready to graph based on geographic regions**

``` r
# finally we can plot according to our colors 

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
```

![](Markdown-for-refugee-network_files/figure-gfm/unnamed-chunk-20-1.png)<!-- -->

We can see that the network appears to be pretty heterogenous: a lot of
different actors from different regions partnering up together. However,
the graph also shows that European and North American actors are
dominating the network.

-----

### There are more ways of exploring the network depending on what type of meta data is available for the nodes. As a next step, we could do some very simple network statisical models to see what influences tie formation in the network.

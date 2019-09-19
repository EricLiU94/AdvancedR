#' determine the shortest path to all the nodes, which are included in the network
#' 
#' @export dijkstra
#' @param graph a data frame, which contains three parameters: the connection path between two nodes and weight function
#' @param init_node from which node, the trip begins 
#' @return the expected shortest journey, towards each and every node, through the entire network 

dijkstra <- function (graph, init_node)  {
  stopifnot(is.data.frame(graph), is.numeric(init_node), colnames(graph)==c("v1", "v2", "w"), nrow(graph) >= init_node)
  nod<- as.matrix(graph[1])
  lank<- as.matrix(graph[2])
  vikt <- as.matrix( graph[3])
  
  # the total number of nodes
  l<- max(nod) 
  l2<- length(nod)
  
  temp_matris<- matrix(1000000000000000000000000000, l, l) 
  
  # to assign the connections to each matrix element 
  for (i in 1:l2) {
    temp1<-nod[i]
    temp2<-lank[i]
    temp3<-vikt[i]
    f<-c(temp1, temp2, temp3)
    
    temp_matris[temp1, temp2]<- temp3
    
  }
  # Initially, each path, towards the final destination is assumed to be super long 
  dest_path<- numeric(l) +10000000000000
  # the path towards the inial node is zero 
  dest_path[init_node]<- 0 
  
  # this variable is used to keep track of the nodes, whilst moving through the netword of nodes
  flag_nodes<-numeric (l) 
  # The inital node is denoted with 100, the rest are deoted as zeros 
  flag_nodes[init_node] <- 100
  #again, this is a comparison parameter 
  nod_min <-  10000 
  #this parameter is used as a marker, to indicate which nodes has just been visited 
  Min_koord<-init_node
  
  ######################################################################################
  # to begin with, the first node is scrutinised by itself, without looping further to the next node 
  test_vec<- temp_matris[init_node,] # current_node: the starting node 
  
  for (i in 1:length(test_vec))  {
    ref_value <- 10000
    # it goes through each and every element along the scrutinised row vector 
    temp_koll<- test_vec[i]
    #it says that if the current node is not a node, which has already been visited before and if the
    # magnitude of the weight function is not super big, that is there is no connection between the two
    # nodes, than the opertion starts 
    if(temp_koll< ref_value && flag_nodes[i] < 100) {
      # if the node could be reached, the flag will be one, instead of zero
      flag_nodes[i] <- 1
      dest_path[i] <- test_vec[i]
      if ( temp_koll< nod_min){
        #print( paste(nod_min, temp_koll))
        nod_min <- temp_koll 
        # Min_koord is simply the littlest dist value along this row, amongst all available nodes 
        Min_koord <- i
      }
    } 
    
  } 
  # after the visit, the littlest node is denoted with 100, that is the nest trip starts from this source node 
  flag_nodes[Min_koord] <- 100
  #########################################################################################################
  # step 2
  # using the same method to loop through the rest of the nodes through the entire network
  
  for (w in 2:l) {
    # to screen over each node 
    test_vec<- temp_matris[Min_koord, ]
    tdest_path<- numeric(l)
    dist_pre<- dest_path[Min_koord] 
    # to find the smallest path, other than the initial path, which is zero 
    # to find the smallest path amongst the multiple available paths up to this point 
    for (i in 1:(l)) {
      ref_value <- 10000
      temp_koll<- test_vec[i]
      if(temp_koll< ref_value && flag_nodes[i] < 100) {
        tdest_path[i] <- test_vec[i]+  dist_pre
        tempd<-  tdest_path[i]
        if (tempd< dest_path[i]) {
          dest_path[i]<- tempd 
        }
      }
      
    }
    
    # I need to look for the next minimum 
    nod_min <-  10000
    for (i in 1:l) {
      tempj<- dest_path[i]
      
      if (tempj< nod_min && flag_nodes[i] < 100 ) {
        nod_min<-tempj
        Min_koord <- i
      }
    }
    flag_nodes[Min_koord] <- 100
  }
  
  return(dest_path)
  
}





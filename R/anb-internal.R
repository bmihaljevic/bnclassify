# all functions with begin wit anb_
# dag2modelstring. modelstring2dag. 
# When buliding a DAG, I will need the parents list per variable.
# dag is a type of adj list where each has only its parents, and also includes itself in the list.
# IF the DAG is topologically sorted, then class cannot be the last CPT. This is not important anyway; not a requirement.
# This is related to the anb-families. It is the anb class. 

# TODO 'families' is just a way to represent the anb that corresponds to the cpts. but could be internal to the anb object. if indeed doing that, i would need to transform from families to adjacency lists or similar.

anb_internal <- function() {
} 
# THIS SHOULD RETURN A c("anb", "dag"), as dag is the more general class. 

anb_make_nb <- function(class, features) {
#   Check class is character and length one, features is length 0 or character,
#   class is not in features.
    check_features(features, class)
#   If > 0 features, add arc from class to each of them
    narcs <- length(features)
    arcs  <- graph_from_to_to_edges(rep(class, narcs), features) 
#   Set nodes as class + features 
    nodes <- c(class, features)
    g <- graph_internal(nodes, arcs) 
    g 
}             
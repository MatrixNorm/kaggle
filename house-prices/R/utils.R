

# XXX
utils.heterogeneity_score = function(vector){
    len = length(vector)
    uniq_len = length(unique(vector))
    100 * (uniq_len / len)
}


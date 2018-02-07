
source('./helpers.R')


kaggle.house <- within(kaggle.house, 
{
    
    utils <- within(list(), 
    {
        get_char_columns_names <- function (df) {
            df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names %>% sort
        }
    })
})

# XXX
utils.heterogeneity_score = function(vector){
    len = length(vector)
    uniq_len = length(unique(vector))
    100 * (uniq_len / len)
}


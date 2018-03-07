
within(list(), 
{
    condition <- quote(GrLivArea > 4000 & !is.na(SalePrice) & SalePrice < 2e5)
    
    remove_outliers <- function(dataset) {
        dataset %>% 
        filter( !(!!condition) )
    }
    
    get_strange_cases <- function(dataset) {
        dataset %>%
        filter(!!condition)
    }
     
})

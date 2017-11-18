
outliers <- within(list(), 
{
    removeOutliers <- function (trainset) {
        trainset %>% filter( !(GrLivArea > 4000 & SalePrice < 2e5) )
    }    
     
})

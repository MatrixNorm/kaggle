

within(kaggle.house, 
{
    get_character_colnames <- function (df) {
        df %>% purrr::map(~is.character(.)) %>% purrr::keep(~.) %>% names
    }
    
    get_numeric_colnames <- function (df) {
        df %>% purrr::map(~is.numeric(.)) %>% purrr::keep(~.) %>% names
    }
})

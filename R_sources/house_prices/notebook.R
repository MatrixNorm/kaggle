
within(list(), {

   inject_css <- function() {
       styles <- "
           .rendered_html thead {
           background: #dde7fa;
           }
           .rendered_html td, th {
           position: relative;
           }
           .rendered_html  td:after, th:after {
           position: absolute;
           border-right: 1px solid #a7a7a7;
           content: '';
           top: 25%;
           bottom: 25%;
           right: -1px;
           }
           .rendered_html tr td:last-child:after, th:last-child:after {
           border-right: 0;
           }
           
           .__matrix_norm {
           display: inline-block;
           margin-right: 40px;
           }
           
           .__matrix_norm li {
           display: inline-block; 
           padding: 2px 4px 2px 4px;
           margin: 3px 3px 3px 3px;
           border: 1px solid #aac9db;
           color: #2e2e2e;
           border-radius: 3px
           }
           
           .__matrix_norm ul {
           list-style: none; 
           padding-left: 0 !important;
           margin-top: 0;
           }
           
           .__matrix_norm i {
           padding: 8px 4px 0 4px;
           display: inline-block;
           margin-bottom: px;
           margin-left: 4px;
           font-size: 0.8em;
           }
       "
       stringr::str_interp("<style>${styles}</style>") %>% 
       (IRdisplay::display_html)
   }
           
   show_table <- function(..., cols = 1, caption = "") {
       list(...) %>%
       purrr::map(function(item) {
           if (is.tibble(item)) {
               show_table.html(item, caption = caption, cols = cols)
           } else {
               df <- item[[1]]
               caption = ifelse(length(item) > 1, item[[2]], "")
               cols = ifelse(length(item) > 2, item[[3]], 1)
               show_table.html(df, caption, cols)
           }
       }) %>%
       paste0(collapse='') %>%
       (IRdisplay::display_html)
   }
       
   show_table.html <- function(df, caption = "", cols = 1) {
       table_html <- repr::repr_html(df)
       stringr::str_interp("
           <div style='display:inline-block; vertical-align: bottom;'>
               <i style='font-size: 0.8em;'>${caption}</i>
               <div style='column-count: ${cols}; padding-right:25px;'>
                    ${table_html}
               </div>
           </div>
       ")
    }
    
    show_list <- function(lst, caption = NULL) {
        show_list.html(lst, caption) %>% 
        (IRdisplay::display_html)
    }
    
    show_list.html <- function(lst, caption = NULL) {
        len <- length(lst)
        if (len > 200) {
            stop("list is too long")
        }
        lst_html <- show_list.html.recur(lst)
        caption_html <- ifelse(is.null(caption), "", stringr::str_interp("<i>${caption}</i>"))
        len_html <- ifelse(length(lst) < 7, "", stringr::str_interp("<i>(${len} elems)</i>"))
        stringr::str_interp("<div class='__matrix_norm'>${len_html}${caption_html}${lst_html}</div>")
    }
    
    show_list.html.recur <- function(lst) {
        ul_template <- "<ul>${li_html}</ul>"
        
        li_html <- 
            lst %>% purrr::map(function(item) {
                if (item %>% length == 1) {
                    li_template <- "<li>${item}</li>"
                    stringr::str_interp(li_template)
                } else {
                    item_html = show_list.html.recur(item)
                    stringr::str_interp("
                                        <li>${item_html}</li>"
                    )
                }
            }) %>% paste0(collapse='')
        
        stringr::str_interp(ul_template)
    }
})

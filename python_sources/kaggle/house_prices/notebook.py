
from IPython.display import Image, display, HTML
from rpy2 import robjects
from rpy2.robjects import pandas2ri
from rpy2.robjects.lib import (grdevices, ggplot2)
from rpy2.robjects.packages import importr


pandas2ri.activate()
gridExtra = importr("gridExtra")


class Plot():
    def __init__(self, width, height, res):
        self.width = width
        self.height = height
        self.res = res
        self.result = []

    def __enter__(self):
        def plot(g):
            self.result.append(g)
        return plot

    def __exit__(self, *args):
        with grdevices.render_to_bytesio(grdevices.png, 
                                         width=self.width, 
                                         height=self.height, 
                                         res=self.res) as img:
            print(self.result.pop())
        display_img(img)


def grid_plot(plots, widths=None, layout_matrix=None, display_params=None):
    
    layout_matrix = robjects.r['rbind'](
        *[robjects.IntVector(row) for row in layout_matrix]
    )
    
    with grdevices.render_to_bytesio(grdevices.png, **display_params) as img:
        gridExtra.grid_arrange(
            *plots, 
            widths=widths,
            layout_matrix=layout_matrix
        )    
    display(Image(data=img.getvalue(), format='png', embed=True))


def display_img(img):
    display(Image(data=img.getvalue(), format='png', embed=True))


def show_table(*dfs, cols=1):
    wrap = """
        <div style="display:inline-block;
                    column-count: {cols}; padding-right:25px;
                    vertical-align: top;">
            {df}
        </div>"""
    divs = [wrap.format(df=df.to_html(), cols=cols) for df in dfs]
    return HTML("".join(divs))


def show_list(lst):
    lst_html = _show_list(lst)
    return HTML(
        """<div class='__matrix_norm'>{}</div>""".format(lst_html)
    )


def _show_list(lst):
    li_template = """<li>{}</li>"""
    ul_template = """<ul>{}</ul>"""
    li_buff = []
    for item in lst:
        if type(item) in (list, tuple):
            li_buff.append(_show_list(item))
        else:
            li_buff.append(li_template.format(item))
    li_html = "".join(li_buff)
    return ul_template.format(li_html)


def inject_css():
    styles = """
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
        }
    """
    return display(HTML("<style>{}</style>".format(styles)))

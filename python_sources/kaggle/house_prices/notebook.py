
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


def show_list(lst, cap=None):
    return HTML(show_list_html(lst, cap=cap))


def show_list_html(lst, cap=None):
    cap_template = """<i>{}</i>"""
    len_template = """<i>({} elems)</i>"""
    lst_html = _show_list_html(lst, cap=cap)
    cap_html = cap_template.format(cap) if cap else ''
    len_html = len_template.format(len(lst)) if len(lst) > 7 else ''
    return (
        """<div class='__matrix_norm'>{}{}{}</div>"""
        .format(len_html, cap_html, lst_html)
    )


def _show_list_html(lst, cap=None):
    li_template = """<li>{}</li>"""
    ul_template = """<ul>{}</ul>"""
    buff = []
    for item in lst:
        if type(item) in (list, tuple):
            buff.append(li_template.format(_show_list_html(*item)))
        else:
            buff.append(li_template.format(item))
    html = ul_template.format("".join(buff))    
    return html


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
    """
    return display(HTML("<style>{}</style>".format(styles)))

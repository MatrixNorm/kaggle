
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
    ul_css = "list-style: none; padding-left: 0"
    li_css = """
        display: inline-block; 
        padding: 2px 4px 2px 4px;
        margin: 3px 3px 3px 3px;
        background: #efefef;
        color: #565656;
        border-radius: 3px
    """
    li_template = """<li style="{li_css}">{}</li>"""
    ul_template = """<ul style="{ul_css}">{}</ul>"""
    li_html = "".join([li_template.format(item, li_css=li_css) for item in lst])
    ul_html = ul_template.format(li_html, ul_css=ul_css)
    display(HTML(ul_html))

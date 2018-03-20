
from IPython.display import Image, display
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


def grid_plot(*plots, widths=None, layout_matrix=None):
    
    layout_matrix = robjects.r['rbind'](
        *[robjects.IntVector(row) for row in layout_matrix]
    )
    
    with grdevices.render_to_bytesio(grdevices.png, width=2000, height=1200, res=120) as img:
        gridExtra.grid_arrange(
            *plots, 
            widths=widths,
            layout_matrix=layout_matrix
        )    
    display(Image(data=img.getvalue(), format='png', embed=True))


def display_img(img):
    display(Image(data=img.getvalue(), format='png', embed=True))


from IPython.display import Image, display
from rpy2.robjects import pandas2ri
from rpy2.robjects.lib import (grdevices, ggplot2)

pandas2ri.activate()


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


def display_img(img):
    display(Image(data=img.getvalue(), format='png', embed=True))

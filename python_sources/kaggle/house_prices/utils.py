
from IPython.display import Image, display


def display_img(img):
    data = img.getvalue()
    display(Image(data=data, format='png', embed=True))
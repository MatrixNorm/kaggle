
def get_categ_colnames(df):
    return [
        col[1].name for col in df.items() if col[1].dtype.kind == 'O'
    ]


def get_numeric_colnames(df):
    # possibly better way
    # https://stackoverflow.com/questions/25039626/how-do-i-find-numeric-columns-in-pandas
    return [
        col[1].name for col in df.items() if col[1].dtype.kind in ('i', 'f')
    ]

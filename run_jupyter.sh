
ROOT=`pwd`
R_SOURCES=$ROOT/R_sources
PY_SOURCES=$ROOT/python_sources
DATA_DIR=$ROOT/data

ROOT=$ROOT R_SOURCES=$R_SOURCES PY_SOURCES=$PY_SOURCES DATA_DIR=$DATA_DIR jupyter notebook --port=$1
#!/bin/bash

if [ "$#" -ne 5 ]; then
    echo "usage run.sh Xmax Ymax NumAnts Runtime Datadir"
    exit
fi

# exit it anything returns non zero
set -e

echo "Xmax: $1"
echo "Ymax: $2"
echo "NumAnts: $3"
echo "Runtime: $4"
echo "Datadir: $5"

DATE=$(date +%s)
TAG="$1x$2x$3_$4_$DATE"
TMPDIR="/tmp/ants_$TAG"
DATADIR=$5/$TAG
INDIVIDUAL_DIR=$DATADIR/individual
VID_TMP_DIR=$DATADIR/vid_tmp

echo "TMPDIR: $TMPDIR"

mkdir $TMPDIR
./sim $1 $2 $3 $TMPDIR $4

mkdir $DATADIR
mkdir $INDIVIDUAL_DIR

mv $TMPDIR/* $INDIVIDUAL_DIR

rmdir $TMPDIR

# run the processing scripts
python vis/aggregate_data.py $DATADIR/ants.csv $INDIVIDUAL_DIR/*

mkdir $VID_TMP_DIR
python vis/static_vis.py $DATADIR/ants.csv $DATADIR $VID_TMP_DIR $1 $2

touch $DATADIR/info
echo "Xmax: $1" >> $DATADIR/info
echo "Ymax: $2" >> $DATADIR/info
echo "NumAnts: $3" >> $DATADIR/info
echo "Runtime: $4" >> $DATADIR/info

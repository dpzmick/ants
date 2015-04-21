#!/bin/bash

if [ "$#" -ne 2 ]; then
    echo "usage run.sh ConfFileName Datadir"
    exit
fi

# exit it anything returns non zero
set -e

if [ ! -f $1 ]; then
    echo $1 does not exist
    exit
fi

TYPE=$(basename $1 .conf)
echo Type: $TYPE
echo Datadir: $2

DATE=$(date +%s)
TAG=$TYPE"_"$DATE
echo TAG: $TAG
TMPDIR="/tmp/ants_$TAG"
DATADIR=$2/$TAG
INDIVIDUAL_DIR=$DATADIR/individual
VID_TMP_DIR=$DATADIR/vid_tmp

echo "TMPDIR: $TMPDIR"

mkdir $TMPDIR
./sim $1 $TMPDIR

mkdir $DATADIR
mkdir $INDIVIDUAL_DIR

mv $TMPDIR/* $INDIVIDUAL_DIR

rmdir $TMPDIR

# run the processing scripts
python vis/aggregate_data.py $DATADIR/ants.csv $INDIVIDUAL_DIR/ants/*

mkdir $VID_TMP_DIR
python vis/static_vis.py $DATADIR/ants.csv $DATADIR $VID_TMP_DIR $1

cp $1 $DATADIR/.

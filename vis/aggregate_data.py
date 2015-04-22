# this script reads from a directory of ant data files and outputs a new file
# containing the aggregated and sorted ant data
import sys
import csv
import heapq
import json

def transform_move(move):
    megasecs  = int(move["time"]["megasecs"])
    secs      = int(move["time"]["secs"])
    microsecs = int(move["time"]["microsecs"])
    ant_id    = int(move["antmove"]["antid"])
    cell_x    = int(move["antmove"]["cell"]["x"])
    cell_y    = int(move["antmove"]["cell"]["y"])

    # wooow big numbers
    big_time = (megasecs * 1000) * 10**6 + (secs * 10**6) + microsecs

    return (big_time, ant_id, cell_x, cell_y)

def transform_weight(weight):
    megasecs  = int(weight["time"]["megasecs"])
    secs      = int(weight["time"]["secs"])
    microsecs = int(weight["time"]["microsecs"])
    cell_x    = int(weight["weight_change"]["cell"]["x"])
    cell_y    = int(weight["weight_change"]["cell"]["y"])
    weight    = float(weight["weight_change"]["new_weight"])

    # wooow big numbers
    big_time = (megasecs * 1000) * 10**6 + (secs * 10**6) + microsecs

    return (big_time, cell_x, cell_y, weight)

if __name__ == "__main__":
    # we need at least two args
    if len(sys.argv) < 3:
        print("usage: ants_file cells_file input_files*")
        sys.exit(1)

    output_ants = sys.argv[1]
    output_cells = sys.argv[2]
    inputs = sys.argv[3:]

    print 'reading data'

    move_events = []
    weight_events = []

    for inpt in inputs:
        f = open(inpt, 'r', 32768)

        file_moves = []
        file_weights = []

        for raw_event in f.xreadlines():
            event = json.loads(raw_event)

            if event.get('antmove', -1) != -1:
                file_moves.append(transform_move(event))

            if event.get("weight_change", -1) != -1:
                file_weights.append(transform_weight(event))

        move_events.append(file_moves)
        weight_events.append(file_weights)

        f.close()

    print 'done reading data'

    print 'writing move data'
    of = open(output_ants, 'w', 32768)

    of.write("time,ant_id,cell_x,cell_y\n")

    start_time = None
    for dat in heapq.merge(*move_events):
        if start_time == None:
            start_time = dat[0]

        time = dat[0]
        ant_id = dat[1]
        cell_x = dat[2]
        cell_y = dat[3]

        s = '%d,%d,%d,%d\n' % (time - start_time, ant_id, cell_x, cell_y)

        of.write(s)

    of.close()

    print 'done writing move data'

    print 'writing cell data'
    of = open(output_cells, 'w', 32768)

    of.write("time,cell_x,cell_y,weight\n")

    start_time = None
    for dat in heapq.merge(*weight_events):
        if start_time == None:
            start_time = dat[0]

        time = dat[0]
        cell_x = dat[1]
        cell_y = dat[2]
        weight = dat[3]

        s = '%d,%d,%d,%f\n' % (time - start_time, cell_x, cell_y, weight)

        of.write(s)

    of.close()

    print 'done writing move data'

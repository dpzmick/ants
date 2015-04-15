# this script reads from a directory of ant data files and outputs a new file
# containing the aggregated and sorted ant data
import sys
import csv
import heapq

def transform_data(ant_data):
    megasecs  = int(ant_data[0])
    secs      = int(ant_data[1])
    microsecs = int(ant_data[2])
    ant_id    = int(ant_data[3])
    cell_x    = int(ant_data[4])
    cell_y    = int(ant_data[5])

    # wooow big numbers
    big_time = (megasecs * 1000) * 10**6 + (secs * 10**6) + microsecs

    return (big_time, ant_id, cell_x, cell_y)

if __name__ == "__main__":
    # we need at least two args
    if len(sys.argv) < 2:
        print("usage: output_file input_files*")
        sys.exit(1)

    output = sys.argv[1]
    inputs = sys.argv[2:]

    datas = []

    done = 0.0
    for inpt in inputs:
        print '\r                              ',
        print '\r' + str((done/(len(inputs)-1)) * 100) + '%',
        f = open(inpt, 'r', 32768)

        c = csv.reader(f, delimiter=',')
        c.next() # skip fieldnames

        file_data = []
        for row in c:
            dat = transform_data(row)
            file_data.append(dat)

        datas.append(file_data)

        done += 1.0
        f.close()

    of = open(output, 'w', 32768)

    of.write("time,ant_id,cell_x,cell_y\n")

    start_time = None
    for dat in heapq.merge(*datas):
        if start_time == None:
            start_time = dat[0]

        time = dat[0]
        ant_id = dat[1]
        cell_x = dat[2]
        cell_y = dat[3]

        s = '%d,%d,%d,%d\n' % (time - start_time, ant_id, cell_x, cell_y)

        of.write(s)

    of.close()

import numpy as np
import sys
import csv
import matplotlib.pyplot as plt

class ScentFrame:
    def __init__(self, x_size, y_size, cell_pixels):
        self.x_size = x_size
        self.y_size = y_size

        self.grid_x_size = x_size * cell_pixels
        self.grid_y_size = y_size * cell_pixels
        self.cell_pixels = cell_pixels

        self.data = np.zeros( (self.grid_x_size, self.grid_y_size, 3), dtype=np.uint8)
        self.clear()

    def clear(self):
        self.data.fill(255)

    def set_scent_level(self, x, y, level):
        gx_start = (x-1) * self.cell_pixels
        gx_end = gx_start + self.cell_pixels

        gy_start = (y-1) * self.cell_pixels
        gy_end = gy_start + self.cell_pixels

        self.data[gx_start:gx_end, gy_start:gy_end] = [255 - level, 0, 0]

    def get_image(self):
        return self.data


if __name__ == "__main__":
    conf_file = sys.argv[2]


    f = open(conf_file, 'r')
    xmax = int(f.readline().strip().split(": ")[1])
    ymax = int(f.readline().strip().split(": ")[1])
    f.next() # numants
    f.next() # runtiem
    f.next() # default weight
    f.next() # empty line
    f.next() # header

    frame = ScentFrame(xmax, ymax, 4)

    reader = csv.reader(f, delimiter=",")
    for row in reader:
        x = int(row[0])
        y = int(row[1])
        w = float(row[2])

        frame.set_scent_level(x,y,w)

    f.close()

    inpt = open(sys.argv[1])

    reader = csv.reader(inpt, delimiter=',')
    reader.next() # skip header

    for row in reader:
        time = int(row[0])
        cell_x = int(row[1])
        cell_y = int(row[2])
        weight = float(row[3])

        frame.set_scent_level(cell_x, cell_y, weight)

    inpt.close()

    plt.imshow(frame.get_image())
    plt.show()


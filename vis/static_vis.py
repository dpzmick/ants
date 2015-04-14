import numpy as np
import moviepy.editor as mpy
import sys
import csv

class AntFrame:
    def __init__(self, x_size, y_size, cell_pixels):
        self.x_size = x_size
        self.y_size = y_size

        self.grid_x_size = x_size * cell_pixels
        self.grid_y_size = y_size * cell_pixels
        self.cell_pixels = cell_pixels

        self.data = np.zeros( (self.grid_x_size, self.grid_y_size, 3), dtype=np.uint8)
        self.data.fill(255)

    def put_ant_at(self, x, y):
        gx_start = (x-1) * self.cell_pixels
        gx_end = gx_start + self.cell_pixels

        gy_start = (y-1) * self.cell_pixels
        gy_end = gy_start + self.cell_pixels

        self.data[gx_start:gx_end, gy_start:gy_end] = [0,0,0]

    def get_image(self):
        return self.data


if __name__ == "__main__":
    if len(sys.argv) != 2:
        print('Usage: python static_vis.py input_file')
        sys.exit(1)

    inpt = open(sys.argv[1])

    reader = csv.reader(inpt, delimiter=',')
    reader.next() # skip header

    frames = []

    ant_locs = {}

    quanta = 10**6 # microseconds
    curr_data_start_time = 0

    for row in reader:
        time = int(row[0])
        ant_id = int(row[1])
        cell_x = int(row[2])
        cell_y = int(row[3])

        if time - curr_data_start_time > quanta:
            curr_data_start_time = time

            frame = AntFrame(100, 100, 4)

            for aid, loc in ant_locs.iteritems():
                frame.put_ant_at(loc[0], loc[1])

            frames.append(frame.get_image())

        # update the data
        ant_locs[ant_id] = (cell_x, cell_y)

    clip = mpy.ImageSequenceClip(frames, fps=25)
    clip.write_videofile("test.mp4", fps=25)

    inpt.close()

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
        self.clear()

    def clear(self):
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

    quanta = 10**4 # microseconds
    fps = 30

    inpt = open(sys.argv[1])

    reader = csv.reader(inpt, delimiter=',')
    reader.next() # skip header

    frames = []

    ant_locs = {}

    curr_data_start_time = 0

    count = 0
    vid_count = 0

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

            count += 1

        # update the data
        ant_locs[ant_id] = (cell_x, cell_y)

        if count == 5000:
            clip = mpy.ImageSequenceClip(frames, fps=fps)
            clip.write_videofile(("test_%d.mp4" % vid_count), fps=fps)

            frames = []

            count = 0
            vid_count += 1

    # write any remaining frames
    clip = mpy.ImageSequenceClip(frames, fps=fps)
    clip.write_videofile(("test_%d.mp4" % vid_count), fps=fps)

    inpt.close()

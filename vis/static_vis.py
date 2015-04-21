import numpy as np
import moviepy.editor as mpy
import sys
import csv
import gc

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
    if len(sys.argv) != 5:
        print('Usage: python static_vis.py input_file output_dir tmp_dir conf_file')
        sys.exit(1)

    dump_dir = sys.argv[2]
    tmp_dir = sys.argv[3]
    conf_file = sys.argv[4]

    f = open(conf_file, 'r')
    xmax = int(f.readline().strip().split(": ")[1])
    ymax = int(f.readline().strip().split(": ")[1])
    f.close()

    memory_limit = 4*1024*1024*1024
    frames_per_clip = memory_limit / (xmax * ymax * 3 * 8)

    print 'Using %d frames per clip' % frames_per_clip

    quanta = 10**5 # microseconds
    fps = 30

    inpt = open(sys.argv[1])

    reader = csv.reader(inpt, delimiter=',')
    reader.next() # skip header

    frames = []
    clips = []

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

            frame = AntFrame(xmax, ymax, 4)

            for aid, loc in ant_locs.iteritems():
                frame.put_ant_at(loc[0], loc[1])

            frames.append(frame.get_image())

            count += 1

        # update the data
        ant_locs[ant_id] = (cell_x, cell_y)

        if count == frames_per_clip:
            clip = mpy.ImageSequenceClip(frames, fps=fps)
            clip_name = "%s/ants_%d.mp4" % (tmp_dir, vid_count)
            clip.write_videofile(clip_name, fps=fps)
            clips.append(clip_name)

            # Make sure to remove references to the data we just loaded
            # Run the GC to cleanup the frames we just created, now that they
            # have been written to a video
            frames = []
            clip = None
            gc.collect()

            # TODO no reason to free then reallocate all the frames, just clear
            # them

            count = 0
            vid_count += 1

    # write any remaining frames
    if len(frames) != 0:
        clip = mpy.ImageSequenceClip(frames, fps=fps)
        clip_name = "%s/ants_%d.mp4" % (tmp_dir, vid_count)
        clip.write_videofile(clip_name, fps=fps)
        clips.append(clip_name)

    inpt.close()

    clips = map(lambda name: mpy.VideoFileClip(name), clips)
    final_clip = mpy.concatenate_videoclips(clips)
    final_clip.write_videofile(("%s/ants.mp4" % dump_dir), fps=fps)

An ant simulation using Erlang.

See proposal for more information (sorry its a docx deal with it).

Requirements:
------------
- Erlang
- Python2
- MoviePy (install with pip)

Usage:
-----
```
make
./run.sh ConfigFile OutputDir
```

Examples of the config files should be given in the confs directory.

There is also a script that can be used to generate config files in the confs directory.

After this you should end up with a new directory in OutputDir containing an
ants.csv file and some video files.

If there is no copy of the configuration file in the directory after the run,
something went wrong.

Probably want to make OutputDir == data or something

Also this might be totally wrong so read the code thats the best point of
reference.

If you want to visualize the scent trails, take a look at vis/scent\_vis.py

I have no idea if the performance of the simulation itself is actually any good, I can generate
quite a few events in a pretty short period of time, but I could probably do quite  bit better.
I don't have a good frame of reference for ant simulation performance.

The python scripts all have pretty abysmal performance, but thats fine,
just don't do a big run unless you have time to kill.

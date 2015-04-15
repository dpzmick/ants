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
./run.sh GridXSize GridYSize NumAnts OutputDir TimeToRunSimulation (seconds)
```

After this you should end up with a new directory in OutputDir containing an
ants.csv file and some video files.

If there is no file called "info" in the subdirectory created in your data
directory, something went wrong.

Probably want to make OutputDir == like data or something

Also this might be totally wrong so read the code thats the best point of
reference.

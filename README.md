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

After this you should end up with a new directory in OutputDir containing an
ants.csv file and some video files.

If there is no copy of the configuration file in the directory after the run,
something went wrong.

Probably want to make OutputDir == data or something

Also this might be totally wrong so read the code thats the best point of
reference.

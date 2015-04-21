def weight(x,y):
    if x == y:
        return 10
    else:
        return 'DEFAULT'

if __name__ == "__main__":
    xmax = 1000
    ymax = 1000
    numants = 500
    runtime = 60
    default = 1

    f = open('confs/generated.conf', "w")
    f.write("Xmax: %d\n" % xmax)
    f.write("Ymax: %d\n" % ymax)
    f.write("NumAnts: %d\n" % numants)
    f.write("Runtime: %d\n" % runtime)
    f.write("DefaultWeight: %d\n" % default)
    f.write("\n")
    f.write("cell_x,cell_y,cell_weight")

    for x in xrange(1,xmax+1):
        for y in xrange(1, ymax+1):
            v = weight(x,y)

            if v != 'DEFAULT':
                f.write("%d,%d,%d\n" % (x,y,v))

    f.close()

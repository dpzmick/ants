# single diag
# def weight(x,y):
#     if x == y:
#         return 100.0
#     else:
#         return 'DEFAULT'

# square in center
# def weight(x, y):
#     if x >= 115 and x <= 135 and y >= 115 and y <= 135:
#         return 100.0
#     else:
#         return 'DEFAULT'

# close to top
def weight(x, y):
    if x >= 115 and x <= 135 and y >= 100 and y <= 120:
        return 100.0
    else:
        return 'DEFAULT'

if __name__ == "__main__":
    xmax = 250
    ymax = 250
    numants = 1000
    runtime = 120
    default = 1.0

    f = open('confs/generated.conf', "w")
    f.write("Xmax: %d\n" % xmax)
    f.write("Ymax: %d\n" % ymax)
    f.write("NumAnts: %d\n" % numants)
    f.write("Runtime: %d\n" % runtime)
    f.write("DefaultWeight: %f\n" % default)
    f.write("\n")
    f.write("cell_x,cell_y,cell_weight\n")

    for x in xrange(1,xmax+1):
        for y in xrange(1, ymax+1):
            v = weight(x,y)

            if v != 'DEFAULT':
                f.write("%d,%d,%f\n" % (x,y,v))

    f.close()

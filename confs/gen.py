# single diag
# def weight(x,y):
#     if x == y:
#         return 100.0
#     else:
#         return 'DEFAULT'

# square in center
# def weight(x, y):
#     if x >= 400 and x <= 600 and y >= 500 and y <= 600:
#         return 100.0
#     else:
#         return 'DEFAULT'

# # close to top
def weight(x, y):

    if x >= 50 and x <= 100 and y >= 50 and y <= 100:
        return 'FOOD'

    if y >= 115 and y <= 135 and x >= 50 and x <= 70:
        return 100.0
    else:
        return 'DEFAULT'

if __name__ == "__main__":
    xmax = 500
    ymax = 1000
    numants = 2000
    runtime = 3600 * 2
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

            if v != 'FOOD' and v != 'DEFAULT':
                f.write("%d,%d,%f\n" % (x,y,v))

            if v == 'FOOD':
                f.write("%d,%d,F\n" % (x,y))

    f.close()

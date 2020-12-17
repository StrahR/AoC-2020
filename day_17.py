ll = eval(open("day_17_2.out", "r").readline())


def lookup(sl, i, j):
    for (l, v) in sl:
        if l == (i, j):
            return v


for sl in ll:
    for j in range(-3, 4):
        for i in range(-3, 4):
            print(lookup(sl, i, j), end='')
        print()
    print()

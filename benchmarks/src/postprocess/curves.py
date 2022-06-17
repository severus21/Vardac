import matplotlib.pyplot as plt
import numpy as np

def gen_curve(data, destfile, **kwargs):
    x = np.array(list(data.keys()))
    y = np.array(list(map(lambda x: x["avg"], data.values())))
    ye = np.array(list(map(lambda x: x["stdev"]/2 if "stdev" in x else 0, data.values())))

    fig, ax = plt.subplots()
    ax.errorbar(x, y, yerr=ye, fmt='o')

    ax.set(**kwargs)
    ax.grid()

    #fig.savefig(destfile)
    plt.show()


def gen_curve2(data1, data2, destfile, **kwargs):
    xs1 = np.array(list(data1.keys()))
    ys1 = np.array(list(map(lambda x: x.avg, data1.values())))
    ye1 = np.array(list(map(lambda x: x.stdev/2, data1.values())))
    xs2 = np.array(list(data2.keys()))
    ys2 = np.array(list(map(lambda x: x.avg, data2.values())))
    ye2 = np.array(list(map(lambda x: x.stdev, data2.values())))

    fig, ax = plt.subplots()
    ax.errorbar(xs1, ys1, yerr=ye1, fmt='o', label="A")
    ax.errorbar(xs2, ys2, yerr=ye2, fmt='o', label="B")
    #ax.plot(xs2, ys2, 'b', label="A")
    #ax.plot(xs1, ys1, 'r', label="B")

    ax.set(**kwargs)

    # ax.grid()
    ax.legend()

    #fig.savefig(destfile)
    plt.show()
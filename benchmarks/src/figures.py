import matplotlib.pyplot as plt
import numpy as np

class Figure:
    def __init__(self, title, xlabel, ylabel, curves, filename=None) -> None:
        self.title = title
        self.filename = filename
        self.curves = curves 

        self.xlabel = xlabel
        self.ylabel = ylabel

    def render(self, **kwargs):
        fig, ax = plt.subplots()

        for curve in self.curves:
            curve.render(ax)

        ax.set(**kwargs)
        ax.set_title(self.title)
        ax.set_xlabel(self.xlabel)
        ax.set_ylabel(self.ylabel)

        ax.grid()
        ax.legend()

        if self.filename:
            fig.savefig(self.filename)
        else:
            plt.show()

class Curve:
    def __init__(self, name, data) -> None:
        self.name = name
        self.data = data 

    def render(self, ax):
        xs = np.array(list(self.data.keys()))
        ys = np.array(list(map(lambda x: x.avg, self.data.values())))
        ye = np.array(list(map(lambda x: x.stdev/2, self.data.values())))
        print(xs)
        print(ys)
        ax.errorbar(xs, ys, yerr=ye, fmt='o', label=self.name)
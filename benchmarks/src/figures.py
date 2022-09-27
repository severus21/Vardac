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

class AbstractPlot:
    def __init__(self, name, data, descriptive_statistics_center, descriptive_statistics_dispersion) -> None:
        self.name = name
        self.data = data 
        self.descriptive_statistics_center = descriptive_statistics_center
        self.descriptive_statistics_dispersion = descriptive_statistics_dispersion

    def compute(self, ax):
        # Center the error if needed
        q = 2 if self.descriptive_statistics_dispersion in ['stdev', 'variance'] else 1

        if(type(self.data) == dict): 
            xs = np.array(list(self.data.keys()))
            ys = np.array(list(map(lambda v: getattr(v, self.descriptive_statistics_center), self.data.values())))
        else:
            xs, ys = self.data
            xs = np.array(xs)
            ys = np.array(ys)

        if self.descriptive_statistics_dispersion == None:
            ye = None
        else:
            ye = np.array(list(map(lambda v: getattr(v, self.descriptive_statistics_dispersion)/q, self.data.values())))

        return xs, ys, ye

class Curve(AbstractPlot):
    def __init__(self, name, data, descriptive_statistics_center, descriptive_statistics_dispersion) -> None:
       super().__init__(name, data, descriptive_statistics_center, descriptive_statistics_dispersion) 

    def render(self, ax):
        xs, ys, ye = self.compute(ax)

        if self.descriptive_statistics_dispersion == None:
            ax.plot(xs, ys, marker='o', label=self.name)
        else:
            ax.errorbar(xs, ys, yerr=ye, marker='o', label=self.name)
import pandas as pd
class BarPlot(AbstractPlot):
    def __init__(self, name, data, descriptive_statistics_center, descriptive_statistics_dispersion) -> None:
       super().__init__(name, data, descriptive_statistics_center, descriptive_statistics_dispersion) 

    def render(self, ax):
        xs, ys, ye = self.compute(ax)
        print(xs)
        if self.descriptive_statistics_dispersion == None:
            ax.bar(xs, ys, label=self.name)
        else:
            ax.bar(xs, ys, yerr=ye, label=self.name)
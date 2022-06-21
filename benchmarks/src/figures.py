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
    def __init__(self, name, data, descriptive_statistics_center, descriptive_statistics_dispersion) -> None:
        self.name = name
        self.data = data 
        self.descriptive_statistics_center = descriptive_statistics_center
        self.descriptive_statistics_dispersion = descriptive_statistics_dispersion

    def render(self, ax):
        # Center the error if needed
        q = 2 if self.descriptive_statistics_dispersion in ['stdev', 'variance'] else 1
        
        xs = np.array(list(self.data.keys()))
        ys = np.array(list(map(lambda v: getattr(v, self.descriptive_statistics_center), self.data.values())))
        if self.descriptive_statistics_dispersion == None:
            ax.plot(xs, ys, fmt='o', label=self.name)
        else:
            ye = np.array(list(map(lambda v: getattr(v, self.descriptive_statistics_dispersion)/q, self.data.values())))
            ax.errorbar(xs, ys, yerr=ye, fmt='o', label=self.name)
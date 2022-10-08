import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
from collections import defaultdict


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
            xs, ys, ye= curve.render()

            if not ye.any() :
                ax.plot(xs, ys, marker='o', label=curve.name)
            else:
                ax.errorbar(xs, ys, yerr=ye, marker='o', label=curve.name)

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

class BarFigure(Figure):
    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def render(self, **kwargs):
        d = defaultdict(list)
        for bar_name, curves in self.curves.items():
            d['title'].append(bar_name)
            for curve in curves:
                xs, ys, ye = curve.render()
                assert(len(ys) == 1)
                assert(len(ye) == 1)
                d[curve.name].append(ys[0])
                d[curve.name+"_sd"].append(ye[0])

        colors = ['b', 'r', 'g']
        df = pd.DataFrame(d)
        y_cols = list(filter(lambda x: x != "title" and not x.endswith("_sd"), list(df.columns)))
        yerr_cols = list(filter(lambda x: x.endswith("_sd"), list(df.columns)))
        
        fig = df.plot.bar(
            x='title', 
            y=y_cols, 
            yerr=df[yerr_cols].T.values, 
            #color=colors, 
            rot=0,
            title = self.title,
            xlabel = "",
            ylabel = self.ylabel 
            ).get_figure()

        if self.filename:
            fig.savefig(self.filename)
        else:
            print("kerz")
            plt.show()

class Curve:
    def __init__(self, name, data, descriptive_statistics_center, descriptive_statistics_dispersion) -> None:
        self.name = name
        self.data = data 
        self.descriptive_statistics_center = descriptive_statistics_center
        self.descriptive_statistics_dispersion = descriptive_statistics_dispersion

    def render(self):
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
            ye = list(map(lambda v: getattr(v, self.descriptive_statistics_dispersion)/q, self.data.values()))
            ye = np.array(ye)

        return xs, ys, ye
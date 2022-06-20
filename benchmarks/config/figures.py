import itertools

from src.models import *
from src.postprocess.postprocess import *


from src.figures import *


class FigureFactory:
    def compare(fig_name, xy, args):
        figures = []
        for x,y in xy:
            curves = []
            for (name, results) in args:
                curves.append(Curve(
                        name,
                        Stats(results).extract(x, y)
                    ))
            figures.append(Figure(
                fig_name + f" ({x},{y})",
                x,
                y,
                curves
            )) 
        return figures

# Example exclude/filter based on config
# results.exclude(run_config__regex='"n": 100000')

FIGURES = list(itertools.chain.from_iterable([
    FigureFactory.compare(
        "TOTO1",
        [("n", "duration"), ("n", "rtt")],
        [
            ("two-jvms", Bench.objects.filter(name="simpl-com-akka-multi-jvm", id=315)[0].results.all()),
            ("one-jvm", Bench.objects.filter(name="simpl-com-jvm-akka", id=69)[0].results.all())
        ]
    ) 
]))
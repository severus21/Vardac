import itertools

from src.models import *
from src.postprocess.postprocess import *


from src.figures import *

# Allowed measure of central tendency
DESCRIPTIVE_STATISTICS_CENTER = set(['mean', 'median', 'min', 'max'])
# Allowed measure of the dispersion/variability
DESCRIPTIVE_STATISTICS_DISPERSION = set([None, 'stdev'])

class FigureFactory:
    def compare(fig_name, xy, args, selector=lambda x: True, ylabeling=None, descriptive_statistics_center='mean', descriptive_statistics_dispersion='stdev'):
        print(f"Compare of {fig_name}")
        assert(descriptive_statistics_center in DESCRIPTIVE_STATISTICS_CENTER)
        assert(descriptive_statistics_dispersion in DESCRIPTIVE_STATISTICS_DISPERSION)
        figures = []
        for xy_parameters in xy:
            x,y = xy_parameters[0], xy_parameters[1]
            curves = []
            for (name, results) in args:
                curves.append(Curve(
                        name,
                        Stats(results).extract(*xy_parameters, selector=selector, ylabeling=ylabeling),
                        descriptive_statistics_center,
                        descriptive_statistics_dispersion
                    ))
            figures.append(Figure(
                fig_name + f" ({x},{y})[{descriptive_statistics_center}, {descriptive_statistics_dispersion}]",
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
    ), 
    FigureFactory.compare(
        "Inlining overhead",
        [("n", "duration"), ("n", "rtt")],
        [
            ("akka-one-jvm", Bench.objects.filter(name="simpl-com-akka-one-jvm", id=320)[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="simpl-com-varda-one-jvm", id=316)[0].results.all()),
            ("inline-one-jvm", Bench.objects.filter(name="simpl-com-varda-inline-one-jvm", id=318)[0].results.all())
        ]
    ),
    FigureFactory.compare(
        "payload",
        [("payload", "duration"),],
        [
            ("akka-one-jvm", Bench.objects.filter(name="mpp-akka-one-jvm").order_by('-pk')[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="mpp-varda-one-jvm").order_by('-pk')[0].results.all()),
        ],
        selector    = (lambda x : x['n'] == 100),
        ylabeling   = lambda s,k: s.extract_metric('pp_size')[k].min,
    ),
    FigureFactory.compare(
        "MS",
        [("vs", "duration")],
        [
            ("akka-one-jvm", Bench.objects.filter(name="ms-akka-one-jvm").order_by('-pk')[0].results.all()),
            ("akka-one-jvm-docker", Bench.objects.filter(name="ms-akka-one-jvm-docker").order_by('-pk')[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="ms-varda-one-jvm").order_by('-pk')[0].results.all()),
            ("varda-one-jvm-docker", Bench.objects.filter(name="ms-varda-one-jvm-docker").order_by('-pk')[0].results.all()),
        ]
    ) 
]))
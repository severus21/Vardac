import itertools

from src.models import *
from src.postprocess.postprocess import *


from src.figures import *

# Allowed measure of central tendency
DESCRIPTIVE_STATISTICS_CENTER = set(['mean', 'median', 'min', 'max'])
# Allowed measure of the dispersion/variability
DESCRIPTIVE_STATISTICS_DISPERSION = set([None, 'stdev'])

class FigureFactory:
    def __init__(self, fig_name, xy, args, selector=lambda x: True, ylabeling=None, descriptive_statistics_center='mean', descriptive_statistics_dispersion='stdev',
        category = lambda x:None):
        assert(descriptive_statistics_center in DESCRIPTIVE_STATISTICS_CENTER)
        assert(descriptive_statistics_dispersion in DESCRIPTIVE_STATISTICS_DISPERSION)

        self.fig_name = fig_name
        self.xy = xy
        self.args = args
        self.selector = selector
        self.ylabeling = ylabeling
        self.descriptive_statistics_center = descriptive_statistics_center
        self.descriptive_statistics_dispersion = descriptive_statistics_dispersion
        self.category = category

    @property
    def title(self):
        return self.fig_name

    def curves(self, xy_parameters):
        curves = []
        for (name, results) in self.args():
            for cat, stats in Stats(results).extract(*xy_parameters, selector=self.selector, ylabeling=self.ylabeling, category=self.category).items():
                curves.append(Curve(
                        name+"_"+str(cat) if cat else name,
                        stats,
                        self.descriptive_statistics_center,
                        self.descriptive_statistics_dispersion
                    ))
        return curves

    def compare(self):
        print(f"Compare of {self.fig_name}")
        figures = []
        for xy_parameters in self.xy:
            x,y = xy_parameters[0], xy_parameters[1]
            curves = self.curves(xy_parameters) 
            figures.append(Figure(
                self.fig_name + f" ({x},{y})[{self.descriptive_statistics_center}, {self.descriptive_statistics_dispersion}]",
                x,
                y,
                curves
            )) 
        return figures


class BarFactory(FigureFactory):

    def __init__(self, *args, **kwargs):
        super().__init__(*args, **kwargs)

    def curves(self, xy_parameters):
        curves = defaultdict(list)
        for bar_name, xs in self.args().items():
            for (name, results) in xs:
                for cat, stats in Stats(results).extract(*xy_parameters, selector=self.selector, ylabeling=self.ylabeling, category=self.category).items():
                    curves[bar_name].append(Curve(
                            name+"_"+str(cat) if cat else name,
                            stats,
                            self.descriptive_statistics_center,
                            self.descriptive_statistics_dispersion
                        ))
        return curves

    def compare(self):
        print(f"Compare of {self.fig_name}")
        figures = []
        for xy_parameters in self.xy:
            x,y = xy_parameters[0], xy_parameters[1]
            curves = self.curves(xy_parameters) 
            figures.append(BarFigure(
                self.fig_name + f" ({x},{y})[{self.descriptive_statistics_center}, {self.descriptive_statistics_dispersion}]",
                x,
                y,
                curves
            )) 
        return figures

# Example exclude/filter based on config
# results.exclude(run_config__regex='"n": 100000')

FIGURES = [
    FigureFactory(
        "payload",
        [("payload", "duration"),("payload", "rtt")],
        lambda: [
            ("akka-one-jvm", Bench.objects.filter(name="mpp-akka-one-jvm").order_by('-pk')[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="mpp-varda-one-jvm").order_by('-pk')[0].results.all()),
        ],
        selector    = (lambda x : x['n'] == 10000),
        ylabeling   = lambda s,k: s.extract_metric('pp_size')[k].min,
        category    = (lambda x : x['n']) #one curve per n
    ),
    FigureFactory(
        "MPP",
        [("n", "duration"), ("n", "rtt")],
        lambda: [
            ("akka-one-jvm", Bench.objects.filter(name="mpp-akka-one-jvm").order_by('-pk')[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="mpp-varda-one-jvm").order_by('-pk')[0].results.all()),
            ("inline-one-jvm", Bench.objects.filter(name="mpp-varda-inline-one-jvm").order_by('-pk')[0].results.all()),
            ("contract-one-jvm", Bench.objects.filter(name="mpp-varda-contract-one-jvm").order_by('-pk')[0].results.all()),

            ("akka-multi-jvms", Bench.objects.filter(name="mpp-akka-multi-jvms").order_by('-pk')[0].results.all()),
            ("varda-multi-jvms", Bench.objects.filter(name="mpp-varda-multi-jvms").order_by('-pk')[0].results.all()),
        ]
    ),
    FigureFactory(
        "MS",
        [("vs", "duration"), ("vs", "rtt")],
        lambda: [
            ("akka-one-jvm", Bench.objects.filter(name="ms-akka-one-jvm").order_by('-pk')[0].results.all()),
            #("akka-one-jvm-docker", Bench.objects.filter(name="ms-akka-one-jvm-docker").order_by('-pk')[0].results.all()),
            ("varda-one-jvm", Bench.objects.filter(name="ms-varda-one-jvm").order_by('-pk')[0].results.all()),
            ("varda-one-jvm-wo-refl", Bench.objects.filter(name="ms-varda-one-jvm-wo-refl").order_by('-pk')[0].results.all()),
            #("varda-one-jvm-docker", Bench.objects.filter(name="ms-varda-one-jvm-docker").order_by('-pk')[0].results.all()),
        ]
    ), 
    BarFactory(
        "YCSB",
        [("threads", "Throughput"), ("threads", "Avg Update Latency"), ("threads", "Avg Read Latency")],
        lambda: {
            'YSCB-a' : [
                #("redis", Bench.objects.filter(name="ycsb-redis").order_by('-pk')[0].results.all()),
                #("kvs-redis", Bench.objects.filter(name="ycsb-kvs-varda-redis").order_by('-pk')[0].results.all()),
                ("kvs-varda-inmemory", Bench.objects.filter(name="ycsb-kvs-varda-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-full-varda-inmemory", Bench.objects.filter(name="ycsb-kvs-full-varda-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-akka-inmemory", Bench.objects.filter(name="ycsb-kvs-akka-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-full-akka-inmemory", Bench.objects.filter(name="ycsb-kvs-full-akka-inmemory").order_by('-pk')[0].results.all()),
            ],
            'YSCB-b' : [
                #("redis", Bench.objects.filter(name="ycsb-b-redis").order_by('-pk')[0].results.all()),
                #("kvs-redis", Bench.objects.filter(name="ycsb-b-kvs-varda-redis").order_by('-pk')[0].results.all()),
                ("kvs-varda-inmemory", Bench.objects.filter(name="ycsb-b-kvs-varda-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-full-varda-inmemory", Bench.objects.filter(name="ycsb-b-kvs-full-varda-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-akka-inmemory", Bench.objects.filter(name="ycsb-b-kvs-akka-inmemory").order_by('-pk')[0].results.all()),
                ("kvs-full-akka-inmemory", Bench.objects.filter(name="ycsb-b-kvs-full-akka-inmemory").order_by('-pk')[0].results.all()),
        ]},
        selector    = (lambda x : x['workload'] == "workloada" or x['workload'] == "workloadb"),
    ) 
]
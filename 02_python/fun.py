import numpy as np
import pandas as pd
import math
from scipy import stats
from plotnine import *

def year_full_levels(x):
    year_levels = sorted(x)
    year_range = [min(year_levels), max(year_levels)]
    year_range_num = list(map(lambda x: list(map(int, x.split("-"))), year_range))

    year_seq = [year_range[0]]
    if year_range_num[0][1] == 1:
        year_seq.append(str(year_range_num[0][0]) + '-2')

    for i in range(year_range_num[0][0] + 1, year_range_num[1][0]):
        year_seq.append(str(i) + '-1')
        year_seq.append(str(i) + '-2')
    if year_range_num[1][1] == 2:
        year_seq.append(str(year_range_num[1][0]) + '-1')
    year_seq.append(year_range[1])

    return year_seq


def binom_CI(x, n):
    if math.isnan(n) or math.isnan(x) or n < x or n == 0:
        return np.nan
    return stats.binomtest(x, n, alternative='two-sided').proportion_ci()


def prop_table(dat, response, grid, by=None):
    # @TODO groupby
    # dat = dat.groupby(by=["Messperiode"])

    def summ(x):
        n = [0] * 6
        for y in x:
            if not math.isnan(y):
                n[0] += 1
                if y == 1:
                    n[1] += 1
                else:
                    n[2] += 1
        n[3] = n[1] / n[0]
        bin_int = binom_CI(n[1], n[0])
        n[4] = bin_int.low
        n[5] = bin_int.high
        return n

    index = ["Messperiode"] if by is None else ["Messperiode", by]
    out = dat.pivot_table(dat, index=index, aggfunc={response: [summ]})

    out["n"] = [x[0] for x in out[response]["summ"]]
    out["n_correct"] = [x[1] for x in out[response]["summ"]]
    out["n_wrong"] = [x[2] for x in out[response]["summ"]]
    out["percent_correct"] = [x[3] for x in out[response]["summ"]]
    out["CI_lo"] = [x[4] for x in out[response]["summ"]]
    out["CI_up"] = [x[5] for x in out[response]["summ"]]
    out = out.drop(columns=[response])

    out.reset_index(inplace=True)

    if len(index) == 1:
        out = out.set_index("Messperiode").reindex(pd.Index(grid["Messperiode"], name="Messperiode")).reset_index()
    else:
        out = out.set_index(index).reindex(pd.Index(grid[index])).reset_index().rename(columns={"level_0": "Messperiode", "level_1": by})

    out["n"] = [0 if math.isnan(x) else x for x in out["n"]]
    out["n"] = out["n"].astype(int)
    return out


def prop_period_fun(dat, response, by=None, filter_crit=None):
    if filter_crit is not None:
        dat = dat[(dat[filter_crit] == "Ja")]
    small_grid = pd.DataFrame(year_full_levels(dat["Messperiode"]), columns=["Messperiode"])

    out = [prop_table(dat, response, small_grid)]

    if by is not None:
        by_list = list(np.unique(list(dat[by])))
        by_list.remove("nan")
        grid = pd.DataFrame([[x, y] for x in year_full_levels(dat["Messperiode"]) for y in by_list],
                            columns=["Messperiode", by])
        dat_by = prop_table(dat, response, grid, by)
        out.append(dat_by)
    return out


def plot_by(dat, fileprefix, facet_var, n_min=10, ncols=2, gray_area=True, width2=16, height2=20):
    agg_dat =  dat[0]
    dat = dat[1]
    for x in ["percent_correct", "CI_lo", "CI_up"]:
        dat[x] = np.where(dat["n"] < n_min, np.nan, dat[x])

    cols1 = pd.DataFrame(data=["lightgray"], columns = ["Spezifische Abteilung"])
    cols2 = pd.DataFrame(data=["lightgray"], columns = ["USZ Durchschnitt"])

    p2 = ggplot(dat, aes(x="Messperiode", y="percent_correct", group=1)) + \
         (geom_area(agg_dat, aes(x = "Messperiode", y = "percent_correct", fill = ["USZ Durchschnitt"])) if gray_area else theme()) + \
         geom_errorbar(aes(ymin="CI_lo", ymax="CI_up"), width=0.1, color="darkgray") + \
         geom_point(aes(color = ["Spezifische Abteilung"])) + \
         geom_line(aes(color = ["Spezifische Abteilung"]), size=0.3) + \
         theme_bw() + \
         (theme(legend_position = "bottom", legend_direction="horizontal", legend_box = "horizontal", legend_box_margin=30) if gray_area else theme(legend_position="none")) + \
         guides(color = guide_legend(nrow = 1)) + \
         scale_y_continuous(breaks=[x / 4 for x in range(5)], limits=[-0.2, 1], labels=(lambda x: list(map(lambda y: '%.2f' % y, x)))) + \
         geom_text(aes(label="n", y=-.2), size=6) + \
         annotate(geom="text", x=1.5, y=-.1, label="Gesamt N", nudge_x=0, size=6) + \
         facet_wrap(facet_var, ncol = ncols, scales='free_x') + \
         ylab("Anteil korrekte Beobachtungen") + \
         scale_color_manual(name=" ", values = {"Spezifische Abteilung":"black"}) + \
         scale_fill_manual(name=" ", values = {"USZ Durchschnitt":"lightgray"}) + \
         theme(plot_margin=0.1, text=element_text(size=8)) + \
         theme(axis_text_x=element_text(angle=90, vjust=1, hjust=-0.1, size=6), subplots_adjust={'hspace': 0.4}) + \
         theme(plot_title=element_text(size=6))
    p2.save(fileprefix + "_by_py.png", width=width2, height=height2, units="cm", scale=5, dpi=400)
    return p2


def plot_agg(dat, fileprefix, n_min=10, width1=8, height1=6, title="", suffix=""):
    for x in ["percent_correct", "CI_lo", "CI_up"]:
        dat[x] = np.where(dat["n"] < n_min, np.nan, dat[x])
    p1 = ggplot(aes(x=dat["Messperiode"], y=dat["percent_correct"], group=1, label=round(dat["percent_correct"], 2))) + \
         geom_errorbar(aes(ymin=dat["CI_lo"], ymax=dat["CI_up"]), width=0.1, color="darkgray") + \
         geom_point() + \
         geom_text(nudge_x=0.3, nudge_y=0.04, size=10) + \
         geom_line(size=0.3) + \
         theme_bw() + \
         scale_y_continuous(breaks=[x / 4 for x in range(5)], limits=[-0.2, 1],
                            labels=(lambda x: list(map(lambda y: '%.2f' % y, x)))) + \
         geom_text(aes(label=dat["n"].map(int), y=-.2), size=10) + \
         annotate(geom="text", x=1.5, y=-.1, label="Gesamt N", nudge_x=0, size=10) + \
         ylab("Anteil korrekte Beobachtungen") + \
         theme(plot_margin=-0.2, text=element_text(size=10)) + \
         theme(axis_text_x=element_text(angle=90, vjust=1, hjust=-0.1)) + \
         ggtitle(title) + theme(plot_title=element_text(size=12))
    p1.save(fileprefix + suffix + "_py.png", width=width1, height=height1, units="cm", scale=5, dpi=400)
    return p1


def plot_prop_period(prop_period, fileprefix, facet_var="Station", n_min=10, ncols=2, width1=8, height1=6, width2=16,
                     height2=20, title="", gray_area=True):
    p2 = None
    suffix = ""
    ag_table = prop_period
    if len(prop_period) > 1:
        p2 = plot_by(prop_period, fileprefix, facet_var, n_min, ncols, gray_area)
        suffix = "_agg"

    p1 = plot_agg(prop_period[0], fileprefix, n_min, width1, height1, title, suffix)
    return [p1, p2]

from fun import *

df1 = pd.read_excel("../01_data/VAP_python_output2_cens.xlsx", engine='openpyxl')

df1["OKH_Formel"] = df1["OKH_Formel"].map({'korrekt': 1, 'nicht korrekt': 0})
df1["Sedationsstopp_Ja_Nein"] = df1["Sedationsstopp_Ja_Nein"].map({'Ja': 1, 'Nein': 0})
df1["Mundpflege"] = df1["Mundpflege"].map({'Ja': 1, 'Nein': 0})

t_OKH = prop_period_fun(df1, "OKH_Formel", "Station", "OKH_istRelevant")
t_Sed = prop_period_fun(df1, "Sedationsstopp_Ja_Nein", "Station", "Sedationsstopp_istRelevant")
t_Mund = prop_period_fun(df1, "Mundpflege", "Station", "Mundpflege_istRelevant")

# print(t_OKH)

p_OKH = plot_prop_period(t_OKH, "../03_figures/01_VAP_OKH")
p_Sed = plot_prop_period(t_Sed, "../03_figures/01_VAP_Sed")
p_Mund = plot_prop_period(t_Mund, "../03_figures/01_VAP_Mund")
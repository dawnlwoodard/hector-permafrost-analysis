import numpy as np

#(0.6818181818181818+0.582375478927203+0.49253731343283585+0.5313901345291481+0.47101449275362317+0.5043478260869565+0.5165562913907285+0.5185185185185185+0.5079006772009029+0.4962962962962963+0.5384615384615384+0.48226950354609927)

ch4 = 0.22
ch4_err = 0.34

co2_ox = 2.6
co2_ox_err = 3.2

co2_anox = 0.17
co2_anox_err = 0.26

tot = ch4+co2_ox+co2_anox
tot_err = np.sqrt(ch4_err**2 + co2_ox_err**2 + co2_anox_err**2)

ch4_frac = ch4/tot

ch4_frac_err = ch4_frac*np.sqrt((ch4_err/ch4)**2+(tot_err/tot)**2)

print tot, tot_err, ch4_frac, ch4_frac_err

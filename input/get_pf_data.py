import process_cmip_funcs as pcf
import os
import pandas as pd
import pickle
import numpy as np

# NOTE: setting these to False requires having CMIP data available for access and stored as indicated in 'folders' with
# subfolders corresponding to each model name and within each of those should be two folders: concatted_files (with the
# full timeseries for each variable) and annual_means (with the each timeseries resampled to an annual mean)
# or else this code should be edited to match actual structure

run_hist = True
run_245 = True
run_585 = True

# exclude models based on missing data
models_to_exclude = ["cesm2", "cmip_means", "ec_earth3", "gfdl_cm4", "bcc_csm2_mr", "e3sm_1_0", "fgoals_f3_1", "ipsl_cm6a_lr", "taiesm1", "ukesm1_0_ll"]
folders = ["cmip6/hist_data", "cmip6/data_future/ssp245", "cmip6/data_future/ssp585"]  # fixed variables such as areacella should be in cmip6/fx_vars

# get c_values, pf area time series, pf frac timeseries, pf lost in 2010 (only need this for historical)
if os.path.exists("store_hist.pickle") and run_hist is False:
    file_to_read = open("store_hist.pickle", "rb")
    hist_out = pickle.load(file_to_read)
    file_to_read.close()
else:
    time_data = pd.date_range(start="1850", end="2015", freq="M")
    hist_out = pcf.calc_pf_output(folders[0], time_data, lost_year=2010, models_to_exclude=models_to_exclude)
    file_to_store = open("store_hist.pickle", "wb")
    pickle.dump(hist_out, file_to_store)
    file_to_store.close()

if os.path.exists("store_ssp245.pickle") and run_245==False:
    file_to_read = open("store_ssp245.pickle", "rb")
    ssp245_out = pickle.load(file_to_read)
    file_to_read.close()
else:
    time_data = pd.date_range(start="2015", end="2101", freq="M")
    ssp245_out = pcf.calc_pf_output(folders[1], time_data, 2100, models_to_exclude)
    file_to_store = open("store_ssp245.pickle", "wb")
    pickle.dump(ssp245_out, file_to_store)
    file_to_store.close()

if os.path.exists("store_ssp245.pickle") and run_585==False:
    file_to_read = open("store_ssp585.pickle", "rb")
    ssp585_out = pickle.load(file_to_read)
    file_to_read.close()
else:
    time_data = pd.date_range(start="2015", end="2101", freq="M")
    ssp585_out = pcf.calc_pf_output(folders[2], time_data, 2100, models_to_exclude)
    file_to_store = open("store_ssp585.pickle", "wb")
    pickle.dump(ssp585_out, file_to_store)
    file_to_store.close()

# get C values for model
c_vals = hist_out["c_values"]
c_vals_world = hist_out["global_c_values"]

c_litter_world = np.mean(c_vals_world["cLitter"])
c_soil_world = np.mean(c_vals_world["cSoil"])
c_veg_world = np.mean(c_vals_world["cVeg"])

c_litter = np.mean(c_vals["cLitter"])
c_soil = np.mean(c_vals["cSoil"])
c_veg = np.mean(c_vals["cVeg"])
c_litter_cv = np.std(c_vals["cLitter"])/c_litter
c_soil_cv = np.std(c_vals["cSoil"])/c_soil
c_veg_cv = np.std(c_vals["cVeg"])/c_veg

f_litter = np.mean(c_vals["cLitter"]/c_vals_world["cLitter"])
f_soil = np.mean(c_vals["cSoil"]/c_vals_world["cSoil"])
f_veg = np.mean(c_vals["cVeg"]//c_vals_world["cVeg"])
f_litter_cv = np.std(c_vals["cLitter"]/c_vals_world["cLitter"])/f_litter
f_soil_cv = np.std(c_vals["cSoil"]/c_vals_world["cSoil"])/f_soil
f_veg_cv = np.std(c_vals["cVeg"]/c_vals_world["cVeg"])/f_veg



# get frac lost in year

test = ssp245_out["pf_frac_mon"]

ssp45_remaining = test.resample("Y").mean()
hist_remaining = hist_out["pf_frac_mon"].resample("Y").mean()
frac_left_2005_2100 = (ssp45_remaining["2100"].values - hist_remaining["2005"].values)/hist_remaining["2005"].values

ssp45_remaining["2015"]

hist_lost = hist_out["pf_lost_year"]
hist_lost_mean = np.mean(hist_lost)

rcp45_lost = ssp245_out["pf_lost_year"]
rcp45_lost_mean = np.mean(rcp45_lost)

rcp85_lost = ssp585_out["pf_lost_year"]
rcp85_lost_mean = np.mean(rcp85_lost)

# get frac values for table

tas_data = hist_out["tas_HL_mon"]
tas_annual = tas_data.resample("Y").mean()
pf_area = hist_out["pf_area_mon"]
pf_area_annual = pf_area.resample("Y").mean()

# add in future data
tas_data_45 = ssp245_out['tas_HL_mon']
tas_annual_45 = tas_data_45.resample("Y").mean()
pf_area_45 = ssp245_out["pf_area_mon"]
pf_area_annual_45 = pf_area_45.resample("Y").mean()

all_tas_45 = tas_annual.append(tas_annual_45)
all_tas_45 = all_tas_45.drop(axis=1, columns="giss_e2_1_g")
all_pf_area_45 = pf_area_annual.append(pf_area_annual_45)
all_pf_area_45 = all_pf_area_45.drop(axis=1, columns="giss_e2_1_g")

all_tas_mon_45 = tas_data.append(tas_data_45)
all_pf_area_mon_45 = pf_area.append(pf_area_45)

mmean_tas_annual = all_tas_45.mean(axis=1)
mmean_pf_area_annual = all_pf_area_45.mean(axis=1)

all_tas_45["model_mean"] = mmean_tas_annual
all_pf_area_45["model_mean"] = mmean_pf_area_annual
all_pf_frac_45 = all_pf_area_45.copy()

for i in all_pf_area_45.columns:
    all_pf_frac_45[i] = all_pf_area_45[i]/all_pf_area_45[i][0]
    all_tas_45[i] = all_tas_45[i] - all_tas_45[i][0]

all_tas_45.to_csv(r"input/cmip6_tas_HL_45.csv", index=True, header=True)
all_pf_area_45.to_csv(r"input/cmip6_pfarea_45.csv", index=True, header=True)
all_pf_frac_45.to_csv(r"input/cmip6_pffrac_45.csv", index=True, header=True)





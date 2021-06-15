import netCDF4 as nc
import numpy as np
from scipy.ndimage.filters import uniform_filter1d
from scipy.ndimage.filters import maximum_filter1d
import numpy.ma as ma
import os
import subprocess
import pandas as pd

# TODO need to handle if multiple files are found.
def get_filename(subfolder, var_name, file_end=""):
    command = "find {} -name {}*{}.nc -maxdepth 1".format(subfolder, var_name, file_end)
    raw_path = subprocess.check_output(command, shell=True)  # this should return only 1 file
    file_path = raw_path.rstrip()
    return file_path


def get_pf_mask(model_name, folder, var_name="tsl"):
    subfolder = folder + "/" + model_name + "/concatted_files"
    filename = get_filename(subfolder, var_name)
    f_tsl_mon = nc.Dataset(filename, 'r')

    # could get by POA - not tsl, not lat, etc
    if (model_name=="cnrm_esm2_1"):
        depth = f_tsl_mon.variables['sdepth'][:]
    elif (model_name=="ipsl_cm6a_lr"):
        depth = f_tsl_mon.variables['solth'][:]
    else:
        depth = f_tsl_mon.variables['depth'][:]

    dzaa_dict = {"cesm2": 19.4, "e3sm_1_0": 22.0, "fgoals_f3_1": 21.7, "ipsl_cm6a_lr": 16.0,
                 "nor_esm2": 18.9, "taiesm1": 22.8}

    # if model is in key values for dzaa dict,
    # then pf_depth is dzaa depth from dict,

    if model_name in dzaa_dict.keys():
        pf_depth = len(depth[depth < dzaa_dict[model_name]])
    else:
        pf_depth = len(depth)

    tsl_mon = f_tsl_mon.variables[var_name][:, 0:pf_depth, :, :]

    if model_name=='nor_esm2' and folder != "cmip6/hist_data":
        depth_len = len(range(0,pf_depth))
        tsl_mon = np.concatenate([np.reshape(tsl_mon[0,:,:,:],(1,depth_len,96,144)),tsl_mon[:,:,:,:]],axis=0)

    tsl_running_max = maximum_filter1d(tsl_mon[:, pf_depth - 1, :, :], 24, axis=0)

    tsl_mask = tsl_running_max > 273.15

    # get ice cover data
    fx_folder = "cmip6/fx_vars"
    fx_subfolder = fx_folder + "/" + model_name
    ice_file = get_filename(fx_subfolder, "sftgif")
    if len(filename) != 0:
        ice_name_only = ice_file.split("/")[-1]
        f_ice = nc.Dataset(ice_file)
        if "fx" in ice_name_only:
            ice_cover = f_ice.variables["sftgif"][:, :]
        else:
            ice_cover = f_ice.variables["sftgif"][0, :, :]  # want ice cover at start of historical period
        ice_mask = ice_cover != 0

    else:
        ice_data = tsl_mon[0, 0, :, :]
        ice_mask = ice_data == -999  # want whole thing to be false

    lat = f_tsl_mon.variables['lat'][:]
    lat_idx = len(lat[lat > 20])

    lat_mask = tsl_running_max.copy()
    lat_mask[:, -lat_idx:, :] = -999
    lat_mask_bool = lat_mask != -999
    full_mask = ma.mask_or(lat_mask_bool, tsl_mask)
    full_mask = ma.mask_or(full_mask, ice_mask)

    masked_tsl = ma.masked_array(tsl_mon[:, 0, :, :], mask=full_mask)
    masked_tsl = masked_tsl/masked_tsl  # need all non-masked values = 1

    return masked_tsl


def get_area_mask(model_name, pf_mask):

    # TODO can improve by searching at any depth for areacella within a folder. take first occurrence.
    folder = "../fx_vars"
    subfolder = folder + "/" + model_name

    areacella = get_area(model_name, subfolder)

    pf_area = pf_mask * areacella

    return pf_area


def get_area(model_name):

    folder = "cmip6/fx_vars"
    subfolder = folder + "/" + model_name
    # TODO can improve by searching at any depth for areacella within a folder. take first occurrence.
    filename = get_filename(subfolder, "areacella")
    if len(filename) != 0:
        f_area = nc.Dataset(filename)
        areacella = f_area.variables['areacella'][:, :]
    else:
        area_file = "calculated_area_"+model_name+".nc"
        command = "find {} -name {}*.nc -maxdepth 1".format(subfolder, "calculated_area")
        raw_path = subprocess.check_output(command, shell=True)
        if len(raw_path) == 0:
            print("file does not exist. calculating")
            filename = get_filename(subfolder, "tsl")
            cmd = "cdo gridarea " + filename + " " + subfolder+"/"+area_file
            out = subprocess.call(cmd, shell=True)
        f_area = nc.Dataset(subfolder+"/"+area_file)
        areacella = f_area.variables['cell_area'][:, :]

    return areacella


def get_pf_area(model_name, folder, pf_mask, freq='mon'):

    pf_area = get_area_mask(model_name, folder, pf_mask)

    pf_area_out = np.sum(pf_area, axis=(1, 2))

    if freq == 'year':
        pf_area_out = get_annual_mean(pf_area_out)

    return pf_area_out


# return model value of detritus, veg, soil in pf region
def get_c_values(model_name, folder, pf_mask, year=2010, end_year=2014):
    idx = end_year-year-1
    subfolder = folder+"/" + model_name + "/annual_means"
    var_names = ["cLitter", "cVeg", "cSoil"]
    masked_area = get_area_mask(model_name, folder, pf_mask)
    masked_area = masked_area[idx,:,:]
    unmasked_area = get_area(model_name, subfolder)
    c_sums = {}
    c_sums_world = {}
    for var_name in var_names:
        var_file = get_filename(subfolder, var_name)
        if len(var_file) != 0:
            c_sums[var_name] = get_total_c(var_file, masked_area, var_name, idx)
            c_sums_world[var_name] = get_total_c(var_file, unmasked_area, var_name, idx)
    return {"pf_c": c_sums, "wrld_c": c_sums_world}


def get_total_c(c_file, area_data, var_name, idx=0):
    f_carbon = nc.Dataset(c_file)

    c_data = f_carbon.variables[var_name][:, :, :]  # units = kg m^-2
    c_data = c_data*1e-12  # convert to Pg C m^-2
    c_total = c_data[idx, :, :] * area_data  # want to check how variable this is and maybe take average around it
    c_sum = np.sum(c_total)
    return c_sum


def get_cmip_tas(model_name, folder, freq='mon'):
    if freq=='mon':
        subfolder = folder + "/" + model_name + "/concatted_files"
    else:
        subfolder = folder + "/" + model_name + "/annual_means"

    filename = get_filename(subfolder, "tas", "12")
    f_tas = nc.Dataset(filename, 'r')
    tas = f_tas.variables['tas'][:, :, :]

    pf_mask = get_pf_mask(model_name, folder)

    tas_masked = tas*pf_mask

    high_lat_mean_tas = ma.mean(tas_masked, axis=(1, 2))

    global_mean_tas = ma.mean(tas, axis=(1, 2))

    return {"global_mean_tas": global_mean_tas, "high_lat_mean_tas": high_lat_mean_tas}


def get_time_data(nc_data_file, freq="orig"):
    time = nc_data_file.variables['time'][:]  # in days since 1850
    time_fmtd = pd.to_datetime(time, unit="D", origin="1850-01-01")
    if freq == "year":
        return np.unique(time_fmtd.year)  # this way I can take monthly data and still get out the annual value I want
    elif freq == "month":
        return time_fmtd.year + time_fmtd.month/12.0
    else:
        return time_fmtd


def get_annual_mean(data):
    # data is 1D array
    annual_mean = np.zeros(len(data)/12)
    for i in range(0, len(data), 12):
        annual_mean[i/12] = np.mean(data[i:i+13])
    return annual_mean


def rolling_mean_along_axis(a, W, axis=-1):
    # a : Input ndarray
    # W : Window size
    # axis : Axis along which we will apply rolling/sliding mean
    hW = W//2
    L = a.shape[axis]-W+1
    indexer = [slice(None) for _ in range(a.ndim)]
    indexer[axis] = slice(hW, hW+L)
    return uniform_filter1d(a, W, axis=axis)[tuple(indexer)]


def calc_pf_output(folder, time_data, lost_year, models_to_exclude=[""]):
    end_idx = len(time_data)
    raw_model_list = filter(os.path.isdir, [os.path.join(folder, f) for f in os.listdir(folder)])

    model_list = [x.split("/")[-1] for x in raw_model_list]
    model_list = [x for x in model_list if x not in models_to_exclude]

    # carbon values as dataframe
    c_values = pd.DataFrame(data=None, index=model_list, columns=["cLitter", "cVeg", "cSoil"])
    global_c_values = pd.DataFrame(data=None, index=model_list, columns=["cLitter", "cVeg", "cSoil"])
    # tas dict by model, model_name is key,
    tas_mon = pd.DataFrame(data=None, index=time_data, columns=model_list)
    tas_HL_mon = pd.DataFrame(data=None, index=time_data, columns=model_list)
    # pf area time series
    pf_area_mon = pd.DataFrame(data=None, index=time_data, columns=model_list)
    # pf frac time series
    pf_frac_mon = pd.DataFrame(data=None, index=time_data, columns=model_list)
    pf_lost_year = pd.DataFrame(data=None, index=model_list, columns=["pf_lost"])

    # run through models and get out ^
    for model_name in model_list:
        print("starting : " + model_name)

        # get pf mask
        pf_mask = get_pf_mask(model_name, folder)

        # get carbon values
        temp_c_vals = get_c_values(model_name, folder, pf_mask)
        c_values.loc[model_name, :] = temp_c_vals["pf_c"]
        global_c_values.loc[model_name, :] = temp_c_vals["wrld_c"]
        print("done with c_values")

        # get tas timeseries
        tas_dict = get_cmip_tas(model_name, folder)
        tas_mon[model_name] = tas_dict["global_mean_tas"][:end_idx]
        tas_HL_mon[model_name] = tas_dict["high_lat_mean_tas"][:end_idx]

        # get pf area timeseries
        temp = get_pf_area(model_name, folder, pf_mask, freq='mon')
        pf_area_mon[model_name] = temp

        # get pf frac timeseries
        pf_frac_mon[model_name] = pf_area_mon[model_name][:end_idx] / float(pf_area_mon[model_name][0])
        print("done with pf frac timeseries")

        # get pf lost in 2010
        pf_frac_annual = pf_frac_mon[model_name].resample("Y").mean()
        frac_lost = 1 - pf_frac_annual
        pf_lost_year.loc[model_name, :] = frac_lost[str(lost_year)].values[0]

    return {"c_values": c_values, "tas_mon": tas_mon, "tas_HL_mon": tas_HL_mon, "pf_area_mon": pf_area_mon,
            "pf_frac_mon": pf_frac_mon, "pf_lost_year": pf_lost_year, "global_c_values": global_c_values}


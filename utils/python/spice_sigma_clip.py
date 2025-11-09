import numpy as np
from scipy.ndimage import generic_filter
from numpy import ma


def _get_numpy_function(data, name):
    """
    Get NaN-aware version of numpy array function if there are any NaNs in data

    Parameters
    ----------
    data: numpy.array
        Data
    name: str
        Numpy function name, in its not NaN-aware version

    Return
    ------
    function
        Numpy function
    """
    if np.isnan(data).any():
        name = "nan" + name
    return getattr(np, name)


def sigma_clip(
    data,
    size,
    sigma=3,
    sigma_lower=None,
    sigma_upper=None,
    maxiters=5,
    centerfunc="median",
    masked=True,
    ret_center=False,
    ret_stddev=False,
):
    """
     Performs sigma-clipping of the input array.

     Parameters
     ----------
    data: numpy.ndarray
        Input array
    size: int or tuple[int]
        Size of the kernel used to compute the running median (or mean) and standard deviation
    sigma: float
        The number of standard deviations to use for both the lower and upper clipping limit.
        This is overriden by `sigma_lower` and `sigma_upper`
    sigma_lower: float
        Low threshold, in units of the standard deviation of the local intensity distribution
    sigmer_upper: float
        High threshold, in units of the standard deviation of the local intensity distribution
    maxiters: int
        Maximum number of iterations to perform
    centerfunc: str
        Method used to estimate the center of the local intensity distribution ("median" (default) or "mean")
    masked: bool
        Return a `numpy.ma.MaskedArray` (default) instead of an `numpy.array`

    Returns
    -------
    numpy.ndarray
        Filtered array, with clipped pixels replaced by the estimated value of the center of the
        local intensity distribution (either median or mean).
    """
    output = np.copy(data)
    if isinstance(size, np.ndarray):
        size = tuple(size.tolist())
    if type(size) is not tuple:
        size = (size,) * data.ndim
    sigma_lower = sigma_lower or sigma
    sigma_upper = sigma_upper or sigma
    maxiters = maxiters or np.inf
    nchanged = 1
    iteration = 0
    while nchanged != 0 and (iteration < maxiters):
        iteration += 1
        center = generic_filter(output, _get_numpy_function(output, centerfunc), size)
        if ret_center and iteration == maxiters:
            return center
        stddev = generic_filter(output, _get_numpy_function(output, "std"), size)
        if ret_stddev and iteration == maxiters:
            return stddev
        diff = output - center
        new_mask = (diff > sigma_upper * stddev) | (diff < -sigma_lower * stddev)
        output[new_mask] = np.nan
        nchanged = np.count_nonzero(new_mask)
    nan = np.isnan(output)
    output[nan] = center[nan] # Last value for center used for filling
    if masked:
        return ma.masked_array(output, mask=nan)
    else:
        return output

    

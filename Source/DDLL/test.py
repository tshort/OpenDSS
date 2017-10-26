import opendssdirect as dss

from opendssdirect._lib.core import is_x64
print(is_x64())
import sys
print(sys.version_info)

dss.dss_lib.InitAndGetYparams.argtypes

import ctypes

def getYsparse():
    hY = ctypes.c_uint64(0)
    ptr_hY = ctypes.POINTER(ctypes.c_uint64)(hY)
    nBus = ctypes.c_uint32(0)
    ptr_nBus = ctypes.POINTER(ctypes.c_uint32)(nBus)
    nNZ = ctypes.c_uint32(0)
    ptr_nNZ = ctypes.POINTER(ctypes.c_uint32)(nNZ)
    return dss.dss_lib.InitAndGetYparams(hY, nBus, nNZ)

print (getYsparse())

dss.run_command("Redirect ../CMD/test/IEEE13Nodeckt.dss")

print (dss.dss_lib.ErrorDesc())

print (getYsparse())

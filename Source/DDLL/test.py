import opendssdirect as dss
import ctypes

from opendssdirect._lib.core import is_x64
print(is_x64())
import sys
print(sys.version_info)

print (dss.dss_lib.InitAndGetYparams.argtypes)

hY1 = ctypes.c_uint64(0)
print (ctypes.sizeof(hY1))
hY2 = ctypes.c_ulong(0)
print (ctypes.sizeof(hY2))

def getYsparse():
    hY = ctypes.c_uint64(0)
    ptr_hY = ctypes.POINTER(ctypes.c_uint64)(hY)
    nBus = ctypes.c_uint32(0)
    ptr_nBus = ctypes.POINTER(ctypes.c_uint32)(nBus)
    nNZ = ctypes.c_uint32(0)
    ptr_nNZ = ctypes.POINTER(ctypes.c_uint32)(nNZ)
    print (hY, ptr_hY, nBus, ptr_nBus, nNZ, ptr_nNZ)
    return dss.dss_lib.InitAndGetYparams(hY, nBus, nNZ)

print (getYsparse())

dss.run_command("Redirect ../CMD/test/IEEE13Nodeckt.dss")

print (dss.dss_lib.ErrorDesc())

print (getYsparse())

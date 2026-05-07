import gc
from Test import My_Class

# Create an instance so that the Ada method descriptor is actually bound and
# the reference-count path for tp_dealloc is exercised when the class is
# collected during shutdown.
obj = My_Class()

# Keep a reference to the unbound descriptor as well.
meth = My_Class.ada_method

# Delete the references to the instance and the unbound descriptor,
# and then force garbage collection to trigger the tp_dealloc
# path for the class and the method descriptor.
del obj
del meth
gc.collect()

print('<=== TEST PASSED ===>')

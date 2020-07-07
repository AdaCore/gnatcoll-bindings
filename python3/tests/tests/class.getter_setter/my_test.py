from Test import My_Class

# pass an integer in the field waiting for a float
m = My_Class(42)
# the returned value should be a float
assert m.get_value() == 42.0
# Computation
m.set_value(m.get_value() / 2 + 0.1)
# Verify the value almost match
assert str(m.get_value()).startswith("21.1")
# the getter and the component should return the same
assert m.get_value() == m.value
print('<=== TEST PASSED ===>')

from Test import My_Class

assert str(list.append.__class__) == "<class 'method_descriptor'>"
assert list.append.__name__ == "append"
assert list.append.__qualname__ == "list.append"

assert str(My_Class.__class__) == "<class 'type'>"
assert My_Class.__name__ == "My_Class"
assert My_Class.__qualname__ == "My_Class"

assert str(My_Class.ada_method.__class__) == "<class 'ada_method_descriptor'>"
assert My_Class.ada_method.__name__ == "ada_method"
assert My_Class.ada_method.__qualname__ == "My_Class.ada_method"

# No qualname for static method, they are not using the ada_method_descriptor
assert str(My_Class.ada_static.__class__) == "<class 'builtin_function_or_method'>"
assert My_Class.ada_static.__name__ == "ada_static"

c = My_Class()
assert str(c.__class__) == "<class 'Test.My_Class'>"
assert str(c.ada_method.__class__) == "<class 'method'>"
assert c.ada_method.__name__ == "ada_method"
assert c.ada_method.__qualname__ == "PyCapsule.ada_method"
assert str(c.ada_static.__class__) == "<class 'builtin_function_or_method'>"
assert c.ada_static.__name__ == "ada_static"
assert c.ada_static.__qualname__ == "PyCapsule.ada_static"

print('<=== TEST PASSED ===>')

from Test import My_Class

try:
    My_Class.raise_error()
    assert False  # Should be unreachable
except Exception as e:
    assert str(e) == "My_Error_Message"
print('<=== TEST PASSED ===>')

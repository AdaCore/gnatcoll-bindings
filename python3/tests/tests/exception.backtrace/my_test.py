import Test


def inner():
    # Call an Ada function from a nested Python frame. The Ada handler
    # will call Python_Backtrace and assert that the result refers back to
    # this file. This exercises the PyFrame_Get_Code / PyFrame_Get_Back
    # reference-counting fix from the commit.
    Test.check_backtrace()


def outer():
    inner()


outer()

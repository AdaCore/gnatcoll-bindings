from e3.os.process import Run
import os
import sys

p = Run([os.path.join('obj', 'test')])
assert 'Hello from python binding' in p.out, f'output was\n:{p.out}'
print('<=== TEST PASSED ===>')

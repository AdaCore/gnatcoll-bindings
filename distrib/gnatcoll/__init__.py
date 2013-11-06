"""
This file makes the GNATCOLL documentation available from GPS
"""



import GPS
from . import runtime

XML = r"""<?xml version="1.0" ?>
<GPS>
  <doc_path>share/doc/gnatcoll</doc_path>
  <documentation_file>
     <name>html/index.html</name>
     <descr>Gnat Reusable Components User's Guide</descr>
     <category>GNAT</category>
     <menu before='About'>/Help/Gnat Components/Gnat Components User's Guide</menu>
  </documentation_file>
</GPS>
"""

GPS.parse_xml(XML)

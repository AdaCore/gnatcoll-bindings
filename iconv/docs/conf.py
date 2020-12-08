# -*- coding: utf-8 -*-
#
# GNATcoll Bindings - Iconv documentation build configuration file

# General information about the project.
project = u'GNATcoll Bindings - Iconv'

# Output file base name for HTML help builder.
htmlhelp_basename = 'GNATcoll-Iconv'

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass
# [howto/manual]).
latex_documents = [
  ('index', 'GNATcoll-Iconv.tex', u'GNATcoll Bindings - Iconv Documentation',
   u'AdaCore', 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'gnatcoll-iconv', u'GNATcoll Bindings - Iconv Documentation',
     [u'AdaCore'], 1)
]

# Bibliographic Dublin Core info.
epub_title = u'GNATcoll Bindings - Iconv'

exec(open('../../docs-common/common_conf.py').read())

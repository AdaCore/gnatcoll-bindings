# -*- coding: utf-8 -*-
#
# GNATcoll Bindings - Syslog documentation build configuration file

# General information about the project.
project = u'GNATcoll Bindings - Syslog'

# Output file base name for HTML help builder.
htmlhelp_basename = 'GNATcoll-Syslogdoc'

# Grouping the document tree into LaTeX files. List of tuples
# (source start file, target name, title, author, documentclass
# [howto/manual]).
latex_documents = [
  ('index', 'GNATcoll-Syslog.tex', u'GNATcoll Bindings - Syslog Documentation',
   u'AdaCore', 'manual'),
]

# One entry per manual page. List of tuples
# (source start file, name, description, authors, manual section).
man_pages = [
    ('index', 'gnatcoll-syslog', u'GNATcoll Bindings - Syslog Documentation',
     [u'AdaCore'], 1)
]


# Bibliographic Dublin Core info.
epub_title = u'GNATcoll Bindings - Syslog'

exec(open('../../docs-common/common_conf.py').read())

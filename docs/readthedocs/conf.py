# -*- coding: utf-8 -*-
"""from recommonmark.parser import CommonMarkParser"""

project = u'simona'
copyright = u'2020. TU Dortmund University, Institute of Energy Systems, Energy Efficiency and Energy Economics, Research group Distribution grid planning and operation '
version = '1.0'
release = '1.0.0'

# General options
needs_sphinx = '1.0'
master_doc = 'index'
pygments_style = 'tango'
add_function_parentheses = True

extensions = ['sphinx.ext.autodoc',
              'sphinxcontrib.plantuml',
              'sphinx.ext.myst_parser']
templates_path = ['_templates']
exclude_trees = ['.build']
source_suffix = ['.rst', '.md']
source_encoding = 'utf-8-sig'
source_suffix = {
    '.rst': 'restructuredtext',
    '.txt': 'markdown',
    '.md': 'markdown',
}
# HTML options
html_theme = 'sphinx_rtd_theme'
html_short_title = "simona"
htmlhelp_basename = 'simona-doc'
html_use_index = True
html_show_sourcelink = False
html_static_path = ['_static']

# PlantUML options
plantuml = 'plantuml'

# -*- coding: utf-8 -*-

project = u'simona'
copyright = u'2023. TU Dortmund University, Institute of Energy Systems, Energy Efficiency and Energy Economics, Research group Distribution grid planning and operation '
version = '1.0'
release = '1.0.0'

# General options
needs_sphinx = '1.0'
master_doc = 'index'
pygments_style = 'tango'
add_function_parentheses = True

extensions = [
    'myst_parser',
    'sphinx.ext.mathjax',
    'sphinx.ext.autosectionlabel',
    'sphinx.ext.autodoc',
    'sphinxcontrib.plantuml',
    'sphinx.ext.intersphinx',
    'hoverxref.extension',
    'sphinxcontrib.bibtex'
]

# Make sure the target is unique
autosectionlabel_prefix_document = True

myst_enable_extensions = ["dollarmath", "amsmath"]

templates_path = ['_templates']
exclude_trees = ['.build']
source_suffix = ['.md']
source_encoding = 'utf-8-sig'


# HTML options
html_theme = 'sphinx_rtd_theme'
html_short_title = "simona"
htmlhelp_basename = 'simona-doc'
html_use_index = False
html_show_sourcelink = False
html_static_path = ['_static']

# PlantUML options
plantuml = 'plantuml'


# Intersphinx for references to external ReadTheDocs
intersphinx_mapping = {
    'psdm': ('https://powersystemdatamodel.readthedocs.io/en/latest/', None),
}

hoverxref_intersphinx = [
    "psdm",
]

# BibTex options
bibtex_bibfiles = ['_static/bibliography/bibAboutSimona.bib',
                   '_static/bibliography/bibtexAll.bib',
                   ]
bibtex_default_style = 'unsrt'


# BibTex Styles
from pybtex.style.formatting.unsrt import Style as UnsrtStyle
from pybtex.style.labels.alpha import LabelStyle as AlphaLabelStyle

class KeyLabelStyle(AlphaLabelStyle):
    def format_label(self, entry):
        label = entry.key
        return label

class CustomStyle(UnsrtStyle):
    default_sorting_style = 'author_year_title'

    def __init__(self, *args, **kwargs):
        super(CustomStyle, self).__init__(*args, **kwargs)
        self.label_style = KeyLabelStyle()
        self.format_labels = self.label_style.format_labels

from pybtex.plugin import register_plugin

register_plugin('pybtex.style.formatting', 'custom', CustomStyle)
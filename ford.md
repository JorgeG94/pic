---
src_dir: src
output_dir: docs/fpm-ford
project: PIC
summary: A set of commonly used modules for HPC
project_github: https://github.com/JorgeG94/pic
project_download: https://github.com/JorgeG94/pic/archive/refs/heads/main.zip
author: Jorge Luis Galvez Vallejo
github: https://github.com/JorgeG94
page_dir: docs/pages
media_dir: docs/images
graph: true
graph_maxnodes: 250
graph_maxdepth: 5
coloured_edges: true
display: public
         private
         protected
source: true
proc_internals: true
sort: permission-alpha
favicon: docs/images/otter.png
print_creation_date: true
extra_mods: iso_fortran_env:https://gcc.gnu.org/onlinedocs/gfortran/ISO_005fFORTRAN_005fENV.html
            tomlf:https://toml-f.github.io/toml-f
creation_date: %Y-%m-%d %H:%M %z
md_extensions: markdown.extensions.toc
               markdown.extensions.smarty
---
{!README.md!}
---

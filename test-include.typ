Typst supports including files with the command
`#include("filename.typ")`. In fact, this paragraph is being included
from the file `test-include.typ`. A lot of effort has been put into
making `typst-preview-mode` support included files in a seamless
manner. Try clicking somewhere in this section in your preview! If you
run `M-x typst-preview-list-active-files`, you'll see a new entry
corresponding to the included file. When you kill the buffer, the
entry is removed. 

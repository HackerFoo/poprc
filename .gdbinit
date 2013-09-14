set confirm 0

define graph
  call make_graph_all("debug.dot")
end

# set $in_hook_stop = 0
# 
# define hook-stop
#   if $in_hook_stop == 0
#     set $in_hook_stop = 1
#     graph
#     set $in_hook_stop = 0
#   end
# end

# define show
#   call (void)printf("cells[%d] = [", $arg0 - cells)
#   call show_one($arg0)
#   call (void)printf(" ]\n")
# end

set print pretty on
set print object on
set print static-members on
set print vtbl on
set print demangle on
set demangle-style gnu-v3
 
python
import sys
import os
if os.path.exists(os.path.expanduser('~/gdb_printers/python')):
  sys.path.insert(0, os.path.expanduser('~/gdb_printers/python'))
  from libstdcxx.v6.printers import register_libstdcxx_printers
  register_libstdcxx_printers (None)
end

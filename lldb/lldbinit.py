import lldb
import os
import shlex

def __lldb_init_module(debugger, internal_dict):
    dbgcall("command script add -f lldbinit.make make")
    dbgcall("command script add -f lldbinit.graph graph")
    dbgcall("command script add -f lldbinit.cleandot cleandot")
    dbgcall("command script add -f lldbinit.diagrams diagrams")
    dbgcall("command script add -f lldbinit.plog plog")
    dbgcall("command script add -f lldbinit.bc bc")
    dbgcall("command script add -f lldbinit.ac ac")
    dbgcall("command script add -f lldbinit.pt pt")
    dbgcall("command script add -f lldbinit.save_trees save-trees")
    dbgcall("command script add -f lldbinit.diff_trees diff-trees")
    dbgcall("breakpoint set --name breakpoint");
    dbgcall("breakpoint command add 1 --python-function lldbinit.breakpoint_hit");
    dbgcall("command alias popr process launch -- -lo lib.ppr tests.ppr -im")
    return

def dbgcall(command):
    res = lldb.SBCommandReturnObject()
    lldb.debugger.GetCommandInterpreter().HandleCommand(command, res)
    return res.GetOutput()

def breakpoint_hit(frame, bp_loc, dict):
    dbgcall("finish")

def make(debugger, command, result, dict):
    os.system("make -j -s BUILD=debugger eval")
    dbgcall("target delete")
    dbgcall("target create \"eval\"")
    dbgcall("breakpoint set --name breakpoint");
    dbgcall("breakpoint command add 1 --python-function lldbinit.breakpoint_hit");

def graph(debugger, command, result, dict):
    dbgcall("p make_graph_all(0)")

def cleandot(debugger, command, result, dict):
    os.system("make clean-dot")

def diagrams(debugger, command, result, dict):
    os.system("make diagrams")

def plog(debugger, command, result, dict):
    dbgcall("p log_print_all()")

def bc(debugger, command, result, dict):
    args = shlex.split(command)
    if len(args) > 0:
        dbgcall("p print_bytecode(&trace_cells[{}], 1)".format(args[0]))

def pt(debugger, command, result, dict):
    args = shlex.split(command)
    if len(args) > 0:
        dbgcall("p print_tree(&cells[{}])".format(args[0]))

def ac(debugger, command, result, dict):
    args = shlex.split(command)
    if len(args) > 0:
        dbgcall('breakpoint set --file cells.c --line 179 --condition "c-cells==({})"'.format(args[0]))

def save_trees(debugger, command, result, dict):
    os.system("cp trees.log trees-saved.log")

def diff_trees(debugger, command, result, dict):
    os.system("colordiff -u -I^# trees-saved.log trees.log | less -r")

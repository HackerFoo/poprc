import lldb
import os

def __lldb_init_module(debugger, internal_dict):
    dbgcall("command script add -f lldbinit.make make")
    dbgcall("command script add -f lldbinit.cleandot cleandot")
    dbgcall("command script add -f lldbinit.diagrams diagrams")
    return

def dbgcall(command):
    res = lldb.SBCommandReturnObject()
    lldb.debugger.GetCommandInterpreter().HandleCommand(command, res)
    return res.GetOutput()


def make(debugger, command, result, dict):
    os.system("make BUILD=debugger eval")
    dbgcall("target delete")
    dbgcall("target create \"eval\"")

def cleandot(debugger, command, result, dict):
    os.system("make clean-dot")

def diagrams(debugger, command, result, dict):
    os.system("make diagrams")

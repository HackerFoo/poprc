#!/usr/bin/python

import lldb
import os
import time

#expr = "pushr popr"
#expr = "[] pushl swap pushr pushl popr swap popr swap popr swap drop"
#expr = "popr swap popr swap drop swap"
#expr = "popr"
#expr = "pushl popr swap drop"
expr = "[1] swap . popr swap drop"
bp_trace_loc = "bc_trace"

def trace_callback(frame, bp_loc, dict):
    thread = frame.GetThread()
    process = thread.GetProcess()
    process.Stop()
    print('making graph...')
    c = frame.EvaluateExpression('c - cells')
    print c
#    frame.EvaluateExpression('print_trace_index()')
    frame.EvaluateExpression('mark_cell(c)')
    frame.EvaluateExpression('make_graph_all(0, 0)')
    process.Continue()

def map_callback(frame, bp_loc, dict):
    thread = frame.GetThread()
    process = thread.GetProcess()
    process.Stop()
    frame.EvaluateExpression('print_trace_index()')
    process.Continue()

def peval(process, cmd):
    print('< ' + cmd + '\n')
    process.PutSTDIN(cmd + '\n')

    # HACK, I know
    time.sleep(0.1)

    out = process.GetSTDOUT(4096)
    print('> ' + out)
    return out

def make_graph(debugger, command, result, internal_dict):
    debugger.SetAsync(True)

    target = debugger.GetSelectedTarget()

    bp_trace = target.BreakpointCreateByName(bp_trace_loc);
    bp_trace.SetScriptCallbackFunction('lldb_popr.trace_callback')

    map_trace = target.BreakpointCreateByName('map_insert');
    map_trace.SetScriptCallbackFunction('lldb_popr.map_callback')
    map_trace = target.BreakpointCreateByName('map_find');
    map_trace.SetScriptCallbackFunction('lldb_popr.map_callback')

    process = target.LaunchSimple(None, None, os.getcwd())

    # turn on graphing
    peval(process, ':g')

    # compile
    peval(process, ':c tst ' + expr)

    # quit
    peval(process, ':q')

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f lldb_popr.make_graph make_graph')

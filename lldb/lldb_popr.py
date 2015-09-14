#!/usr/bin/python

import lldb
import os
import time

# Set the path to the executable to debug
bp_trace_loc = "bc_trace"

def trace_callback(frame, bp_loc, dict):
    thread = frame.GetThread()
    process = thread.GetProcess()
    process.Stop()
    print('making graph...')
    frame.EvaluateExpression('mark_cell(c)')
    frame.EvaluateExpression('make_graph_all(0)')
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
    process = target.LaunchSimple(None, None, os.getcwd())

    # turn on graphing
    peval(process, ':g')

    # compile
    peval(process, ':c tst popr')

    # quit
    peval(process, ':q')

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand('command script add -f lldb_popr.make_graph make_graph')

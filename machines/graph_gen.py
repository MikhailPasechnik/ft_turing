from graphviz import Digraph
import sys
import json
import os

c = json.load(open(sys.argv[1], 'r'))

g = Digraph(c['name'], filename=os.path.splitext(sys.argv[1])[0] + '.gv')

g.attr('node', shape='doublecircle')
for n in [c['initial']]+c['finals']:
    g.node(n)

g.attr('node', shape='circle')

for instate, trs in c['transitions'].items():
    for t in trs:
        if t['read'] == ' ':
            t['read'] = "' '"
        if t['write'] == ' ':
            t['write'] == "' '"
        g.edge(instate, t['to_state'], label=f"{t['read']}/{t['write']} {t['action'][0]}")

g.view()


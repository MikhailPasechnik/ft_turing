from graphviz import Digraph
import sys
import json
import os


if __name__ == "__main__":
    c = json.load(open(sys.argv[1], 'r'))
    out = sys.argv[2] if len(sys.argv) > 2 else os.path.splitext(sys.argv[1])[0] + '.gv'

    g = Digraph(c['name'], filename=out)

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


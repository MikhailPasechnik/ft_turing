import json
import sys
def check(cfg):
    al = cfg['alphabet']
    for st in cfg['transitions'].values():
        for t in st:
            assert t['read'] in al, f"{t['read']} read not in {al}"
            assert t['write'] in al, f"{t['write']} write not in al"
            assert t['to_state'] in cfg['states'], f"{t['to_state']} not in {cfg['states']}"
    return cfg


check(json.load(open(sys.argv[1])))

# /bin python3
import json

def st(read, to_state, write, action):
    return dict(read=read, to_state=to_state, write=write, action=action)
def fl(l, *c):
    return list(filter(lambda v: v not in c, l))
def gen(l=None):
    L = "LEFT"
    R = "RIGHT"
    alphabet = "".join(map(chr, range(97, 97+26)))
    if l:
        alphabet = alphabet[:l]
    if "n" not in alphabet:
        alphabet+="n"
    if "y" not in alphabet:
        alphabet+="y"
    transitions = [
        (
            "init", [
                st(c, f"match_{c}_right", ".", R) for c in alphabet
            ] + [
                st(".", "reset", ".", R)
            ]
        ),
    ] + [
        (
            f"match_{c}_right", [
                st(cc, f"match_{c}_right", cc, R) for cc in alphabet
            ] + [
                st(".", f"is_{c}", ".", L)
            ]
        ) for c in alphabet
    ] + [
        (
            f"is_{c}", [
                st(c, f"swap_{c}", c, R),
                st(".", f"finish", c, R)
            ] + [
                st(f, f"init_fail_{c}", f, L) for f in fl(alphabet, c)
            ]
        ) for c in alphabet
    ] + [
        (
            f"init_fail_{c}", [
                st(cc, f"init_fail_{c}", cc, L) for cc in alphabet
            ] + [
                st(".", "fail", c, R)
            ]
        ) for c in alphabet
    ] + [
        (
            "fail", [
                st(c, "fail", c, R) for c in alphabet
            ] + [
                st(".", "recover", ".", R)
            ]
        )
    ] + [
        (
            "recover", [
                st(c, f"recover_{c}", ".", L) for c in alphabet
            ] + [
                st(".", "no", ".", L)
            ]
        )
    ] + [
        (
            f"recover_{c}", [
                st(".", "fail", c, R)
            ]
        ) for c in alphabet
    ] + [
        (
            "no", [
                st(".", "HALT", "n", R)
            ]
        )
    ] + [
        (
            f"swap_{c}", [
                st(c, f"match_{c}_left", ".", L),
                st(".", f"swap_{c}", c, L)
            ]
        ) for c in alphabet
    ] + [
        (
            f"match_{c}_left", [
                st(cc, f"match_{c}_left", cc, L) for cc in alphabet
            ] + [
                st(".", "init", c, R)
            ]
        ) for c in alphabet
    ] + [
        (
            "finish", [
                st(".", "reset", ".", R)
            ]
        )
    ] + [
        (
            "reset", [
                st(c, f"reset_{c}", ".", L) for c in alphabet
            ] + [
                st(".", "yes", ".", L)
            ]
        )
    ] + [
        (
            f"reset_{c}", [
                st(".", "finish", c, R)
            ]
        ) for c in alphabet
    ] + [

        (
            "yes", [
                st(".", "HALT", "y", R)
            ]
        )
    ]
    return dict(
        name = "palindrom",
        alphabet = list(alphabet) + ["."],
        blank = ".",
        states = list(dict(transitions).keys()) + ["HALT"],
        initial = "init",
        finals = ["HALT"],
        transitions = dict(transitions),
    )

def check(cfg):
    al = cfg['alphabet']
    for st in cfg['transitions'].values():
        for t in st:
            assert t['read'] in al, f"{t['read']} read not in {al}"
            assert t['write'] in al, f"{t['write']} write not in al"
            assert t['to_state'] in cfg['states'], f"{t['to_state']} not in {cfg['states']}"
    return cfg

if __name__ == "__main__":
    import sys
    ln = None
    if len(sys.argv) > 1:
        ln = int(sys.argv[1], 10)
    print(json.dumps(check(gen(ln)), indent=4))


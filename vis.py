#!/usr/bin/env python3

import sys
import math

try:
    from tkinter import *
except ImportError:
    print('tkinter not found.. abort operation..')
    sys.exit(1)


class F(Frame):
    state = None
    def draw(self, state, pivot, w, b, ttl):
        [w.destroy() for w in self.winfo_children()]
        self.master.title(ttl)
        self.pack(fill=BOTH, expand=1)
        canvas = Canvas(self, bg='gray')
        for i, c in enumerate(state):
            x = i*w+b
            y = 0
            color = "#8f8f8f"
            if pivot == i:
                color = "#c95562"
            else:
                color = "#dedede"
            canvas.create_rectangle(x, y, i*w + w, w + w, outline="#ababab", fill=color)
            canvas.create_text(x+(w - b)/2, b+(w - b)/2, text=f"{c}", font=("DejaVu Math TeX Gyre", int(w/1.5)), justify=CENTER)
        canvas.pack(fill=BOTH, expand=1)
import re

def main():
    if len(sys.argv) > 1 and sys.argv[1].isdigit():
        delay = int(sys.argv[1])
    else:
        delay = 1000
    root = Tk()
    frame = F()
    stack = []
    main.pointer = -1
    ansi_clean = re.compile(r'''\x1B(?:[@-Z\\-_]|\[[0-?]*[ -/]*[@-~])''', re.VERBOSE)
    def update(key):
        try:
            cha = key.char if key else 'd'
            if  main.pointer in (-1, len(stack)-1) and cha == 'd':
                rd = sys.stdin.readline()
                sp = rd.split('|')
                inpO = sp[1]
                at = ansi_clean.sub('\033', inpO).find('\033')
                state = ansi_clean.sub('', inpO)
                stack.append((state, at, sp))

            if cha == 'd':
                main.pointer += 1
            else:
                main.pointer -= 1
            main.pointer = main.pointer if main.pointer >= 0 else 0


            state, at, sp = stack[main.pointer]
            w = min(100, int(1920 / len(state)))
            b = int(w/10)
            #w = w - b * (len(state) - 2)
            print('|'.join(sp), end='')
            root.geometry(f"1920x{w + b}")
            frame.draw(state, at, w, b, ansi_clean.sub('', '|'.join(sp[2:])))
        except Exception as e:
            return

    root.bind('<d>', update)
    root.bind('<a>', update)
    update(None)
    mainloop()

if __name__ == '__main__':
    main()

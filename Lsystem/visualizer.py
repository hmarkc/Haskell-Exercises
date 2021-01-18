import turtle
import sys
from math import inf

def untrace(scale_factor, lines):
    # Hacky, really, we'd like to untrace back to regular turtle commands :(
    for ((x1, y1), (x2, y2), col) in lines:
        turtle.penup()
        turtle.goto(x1 * scale_factor, y1 * scale_factor)
        turtle.pencolor(col)
        turtle.pendown()
        turtle.goto(x2 * scale_factor, y2 * scale_factor)

def max_size(line):
    minx = inf
    maxx = 0
    miny = inf
    maxy = 0
    for ((x1, y1), (x2, y2), _) in lines:
        minx = min(x1, x2, minx)
        maxx = max(x1, x2, maxx)
        miny = min(y1, y2, miny)
        maxy = max(y1, y2, maxy)
    return ((minx, miny), (maxx, maxy))

if __name__ == "__main__":
    lines = eval(sys.argv[1]
            if len(sys.argv) > 1
            else turtle.textinput("Input", "Please paste your [ColouredLine] here: "))
    ((minx, miny), (maxx, maxy)) = max_size(lines)
    x = turtle.window_height()
    scale_factor = x / max((maxx - minx), (maxy - miny))
    turtle.setup(width=x, height=x)
    turtle.setworldcoordinates(minx * scale_factor * 1.01,
                               miny * scale_factor * 1.01,
                               maxx * scale_factor * 1.01,
                               maxy * scale_factor * 1.01)
    turtle.colormode(1)
    turtle.speed("fastest")
    turtle.delay(0)
    untrace(scale_factor, lines)
    input("Press ENTER to exit")
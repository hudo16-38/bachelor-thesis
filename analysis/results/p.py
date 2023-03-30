f = "t7.txt"

with open(f, "r") as file:
    for l in file:
        s1, s2 = l.strip().split()
        o, f, vs = s1.split(":")
        print(s2)

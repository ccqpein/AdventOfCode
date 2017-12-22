def go(basea, baseb, iters):
    basea = basea
    baseb = baseb
    judge = 0
    for i in range(iters):
        basea = basea * 16807 % 2147483647
        baseb = baseb * 48271 % 2147483647
        if basea & 0xFFFF == baseb & 0xFFFF:
            judge += 1
    return judge

if __name__ == "__main__":
    print(go(699, 124, 40000000))

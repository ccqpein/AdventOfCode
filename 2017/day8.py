def solution():
    dic = {}

    def judge(line):
        if line[-2] == "<=":
            return dic[line[-3]] <= int(line[-1])
        elif line[-2] == "<":
            return dic[line[-3]] < int(line[-1])
        elif line[-2] == "==":
            return dic[line[-3]] == int(line[-1])
        elif line[-2] == ">":
            return dic[line[-3]] > int(line[-1])
        elif line[-2] == ">=":
            return dic[line[-3]] >= int(line[-1])
        elif line[-2] == "!=":
            return dic[line[-3]] != int(line[-1])

    def run_command(line):
        if line[1] == "inc":
            dic[line[0]] += int(line[2])
        elif line[1] == "dec":
            dic[line[0]] -= int(line[2])

    with open("./day8.input") as f:
        for line in f:
            temp = line.split(" ")
            dic[temp[0]] = 0

    with open("./day8.input") as f:
        for line in f:
            temp = line.split(" ")
            if judge(temp):
                run_command(temp)

    # return dic
    largest = 0
    for key, val in dic.items():
        if val > largest:
            largest = val

    return largest

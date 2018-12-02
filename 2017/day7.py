def solution():
    dic = {}
    start = ''
    with open("./day7.input") as f:
        for line in f:
            temp = line.split(" ")
            if len(temp) < 3:
                continue
            temp.reverse()
            for ele in temp:
                if ele == '->':
                    break
                clean_word = ele.split(",")[0].split("\n")[0]
                try:
                    dic[clean_word].append(temp[-1])
                except KeyError:
                    dic[clean_word] = [temp[-1]]

                start = clean_word

    # return dic
    while True:
        try:
            start = dic[start][0]
        except:
            break

    return start

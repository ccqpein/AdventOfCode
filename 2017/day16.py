result = {ind: "abcdefghijklmnop"[ind] for ind in range(16)}
store_char_position = {"abcdefghijklmnop"[ind]: ind for ind in range(16)}


def solution(strLis):
    begin_pos = 0
    for single in strLis:
        command = single[0]
        if command == 's':
            begin_pos += int(single[1])
            if begin_pos >= 16:
                begin_pos -= 16
        elif command == 'x':
            parameter = single[1:].split('/')
            first = int(parameter[0]) + begin_pos
            if first >= 16:
                first -= 16
            second = int(parameter[1]) + begin_pos
            if second >= 16:
                second -= 16

            temp = result[first]
            result[first] = result[second]
            result[second] = temp

            store_char_position[result[first]] = first
            store_char_position[result[second]] = second

        elif command == 'p':
            parameter = single[1:].split('/')
            first = store_char_position[parameter[0]]
            second = store_char_position[parameter[1]]

            temp = result[first]
            result[first] = result[second]
            result[second] = temp

            store_char_position[result[first]] = first
            store_char_position[result[second]] = second

    print(f"this is begin_pos {begin_pos}")
    print(result)

if __name__ == "__main__":
    with open("./day16.input") as f:
        line = f.readline()
        solution(line.split(","))

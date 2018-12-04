import os
import datetime
from datetime import date
from statistics import mode
cwd = os.getcwd()


def read_to_cache(filepath):
    cache = []
    with open(cwd + '/' + filepath) as f:
        line = f.readline()
        while line:
            cache.append(line)
            line = f.readline()
    return cache


def demo_list(l):
    result = []
    for i in range(1, len(l), 2):
        result = result + list(range(l[i - 1], l[i]))

    return result


def day4(filepath):
    cache = [this.split(' ') for this in read_to_cache(filepath)]
    date_guard_map = {}
    date_and_sleep_timestamp = {}
    guard_and_sleep_time = {}
    guard_whole_sleep_time = {}

    for entry in cache:
        if entry[2] == "Guard":
            date_guard_map[(entry[0][1:], entry[1][:-1])] = entry[3]
        else:
            if date.fromisoformat(entry[0][1:]) in date_and_sleep_timestamp:
                date_and_sleep_timestamp[date.fromisoformat(
                    entry[0][1:])] += [int(entry[1][:-1][-2:])]
            else:
                date_and_sleep_timestamp[date.fromisoformat(entry[0][1:])] = [
                    int(entry[1][:-1][-2:])]

    for k, v in date_guard_map.items():
        guard_sleep_date = None

        if k[1][0:2] == "23":
            guard_sleep_date = date.fromisoformat(
                k[0]) + datetime.timedelta(days=1)
        else:
            guard_sleep_date = date.fromisoformat(k[0])

        try:
            truely_sleep_time = date_and_sleep_timestamp[guard_sleep_date]
        except KeyError:
            continue

        truely_sleep_time.sort()

        if v in guard_and_sleep_time:
            guard_whole_sleep_time[v] += sum([truely_sleep_time[i] - truely_sleep_time[i - 1]
                                              for i in range(1, len(truely_sleep_time), 2)])
            guard_and_sleep_time[v] += demo_list(truely_sleep_time)

        else:
            guard_whole_sleep_time[v] = sum([truely_sleep_time[i] - truely_sleep_time[i - 1]
                                             for i in range(1, len(truely_sleep_time), 2)])
            guard_and_sleep_time[v] = demo_list(truely_sleep_time)

    who_sleep_most = max(guard_whole_sleep_time.items(), key=lambda k: k[1])[0]

    # return date_guard_map, date_and_sleep_timestamp, guard_whole_sleep_time, guard_and_sleep_time
    day4_part2(guard_and_sleep_time)
    return mode(guard_and_sleep_time[who_sleep_most]), int(who_sleep_most[1:])


def day4_part2(guard_and_sleep_time):
    guard_mode_sleep = {}
    for k, v in guard_and_sleep_time.items():
        # print(v)
        try:
            mode(v)
        except:
            continue

        if mode(v) in guard_mode_sleep:
            guard_mode_sleep[mode(v)] += [(k, v.count(mode(v)))]
        else:
            guard_mode_sleep[mode(v)] = [(k, v.count(mode(v)))]

    # print(guard_mode_sleep)
    for k, v in guard_mode_sleep.items():
        if len(v) == 1:
            # print which minutes, guade number, and how many times he sleep in this minute
            print(k, v)

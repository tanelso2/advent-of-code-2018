from itertools import cycle

with open('input.txt', 'r') as f:
    data = f.readlines()

data = [int(x) for x in data if x != '\n']


def part1(data):
    return sum(data)


def part2(data):
    numbers = set()
    curr_freq = 0
    for x in cycle(data):
        curr_freq += x
        if curr_freq in numbers:
            return curr_freq
        numbers.add(curr_freq)


print(part1(data))
print(part2(data))

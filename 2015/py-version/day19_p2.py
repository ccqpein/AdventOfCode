from random import shuffle

import os

current_file_path = os.getcwd()

transforms = []
molecule = ''
with open(current_file_path + '/2015/inputs/day19.input') as fd:
    lines = [line.strip() for line in fd]
    for line in lines:
        if '=>' in line:
            frm, _, to = line.split()
            transforms.append((frm, to))
        else:
            molecule = line

count = shuffles = 0
mol = molecule

print(mol)
print(transforms)

while len(mol) > 1:
    start = mol
    for frm, to in transforms:
        while to in mol:
            count += mol.count(to)
            mol = mol.replace(to, frm)

    if start == mol:  # no progress start again
        shuffle(transforms)
        mol = molecule
        count = 0
        shuffles += 1

print('{} transforms after {} shuffles'.format(count, shuffles))

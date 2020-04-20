name = input("Enter file:")
handle = open(name)
hours = list()
counts = dict()

for line in handle:
    if not line.startswith("From "):
        continue
    splt1 = line.split()
    time = splt1[5]
    splt2 = time.split(":")
    hours.append(splt2[0])
for hour in hours:
    counts[hour] = counts.get(hour, 0) + 1

for (key, val) in sorted(counts.items()):
    print(key, val)

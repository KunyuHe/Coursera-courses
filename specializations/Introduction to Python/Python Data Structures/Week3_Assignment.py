# Use the file name mbox-short.txt as the file name
fname = input("Enter file name: ")
fh = open(fname)
c=0
d=0
for line in fh:
    if not line.startswith("X-DSPAM-Confidence:") : continue
    line = line.rstrip()
    a=line[19:]
    b=float(a.lstrip())
    c=c+b
    d=d+1
avg=c/d
print("Average spam confidence:", avg)

import time

metrics = 10000
values = 1000000

v = 0.0

time.sleep(3)

for v in range(values):
    print("m%d %d.0" % (v % metrics, v + 1))
for m in range(metrics):
    print("? m%d" % m)

print("!")
print("!")
print("!")

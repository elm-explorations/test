#!/usr/bin/env python3
import sys
import matplotlib.pyplot as plt

data = [int(line.strip()) for line in open(sys.argv[1]) if line.strip()]

plt.hist(data, bins='auto', edgecolor='black')
plt.xlabel('Tries needed')
plt.ylabel('Count')
plt.title('Distribution of tries needed to find all bugs')
plt.tight_layout()
plt.savefig('tries_histogram.png', dpi=150)
print(f"Saved to tries_histogram.png  (n={len(data)}, mean={sum(data)/len(data):.1f})")

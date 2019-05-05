import random

def sort(inp):
	sorted_output = []
	for e in iter(inp):
		idx = 0
		while sorted_output and idx < len(sorted_output) and e > sorted_output[idx]:
			idx += 1
		sorted_output.insert(idx, e)
	return sorted_output

if __name__ == '__main__':
	mylist = []
	for i in range(10000):
		mylist.append(random.randint(0,10000))
	sort(mylist)


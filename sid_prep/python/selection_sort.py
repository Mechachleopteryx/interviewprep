import random

def sort(inp):
	for sorted_idx in range(0,len(inp)-1):
		for unsorted_idx in range(sorted_idx+1,len(inp)):
			if inp[unsorted_idx] < inp[sorted_idx]:
				temp = inp[sorted_idx]
				inp[sorted_idx] = inp[unsorted_idx]
				inp[unsorted_idx] = temp
	return inp

if __name__ == '__main__':
	mylist = []
	for i in range(10000):
		mylist.append(random.randint(0,10000))
	sort(mylist)


import random

def sort(inp):
	n = len(inp)
	if n == 0 or n == 1:
		return inp
	l,r = sort(inp[0:n/2]), sort(inp[n/2:])

	merged_list = []
	if not l:
		return r
	if not r:
		return l
	e_l, e_r = l.pop(0), r.pop(0)
	while True:
		if e_l <= e_r:
			merged_list.append(e_l)
			if l:
				e_l = l.pop(0)
			else:
				merged_list.extend([e_r] + r)
				break
		else:
			merged_list.append(e_r)
			if r:
				e_r = r.pop(0)
			else:
				merged_list.extend([e_l] + l)
				break

	return merged_list

if __name__ == '__main__':
	print sort([1,4,2,6,5,3])
	mylist = []
	for i in range(10000):
		mylist.append(random.randint(0,10000))
	sort(mylist)


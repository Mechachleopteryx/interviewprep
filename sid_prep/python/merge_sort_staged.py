import random

def elements(inp):
	n = len(inp)
	if n == 1:
		return [inp]
	l,r = inp[0:n/2], inp[n/2:]
	return elements(l) + elements(r)

def merge(l,r):
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

def sort(inp):
	if len(inp) == 0:
		return inp
	els = elements(inp)
	i = 0
	while len(els) > 1:
		if i >= len(els)-1:
			i = 0
		l,r = els.pop(i), els.pop(i)
		els = [merge(l,r)] + els
		i += 1
	return els[0]


if __name__ == '__main__':
	print elements([1,2,3,4,5])
	print merge([1,2,5],[2,6,7])
	print sort([1,4,2,6,5,3])
	mylist = []
	for i in range(10000):
		mylist.append(random.randint(0,10000))
	sort(mylist)


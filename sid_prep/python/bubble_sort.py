import functools
import random

def default_cmpr(a, b):
	return a < b

def ensure_default_comparator(func):
	@functools.wraps(func)
	def wrapper(*args, **kwargs):
		if len(args) == 1 and 'cmpr' not in kwargs:
			kwargs['cmpr'] = default_cmpr
		return func(*args, **kwargs)
	return wrapper

@ensure_default_comparator
def sort(inp, cmpr=None):
	""" Generalized to sort by an arbitrary comparator instead of
	just numeric ordering. Numeric ordering used as default. """
	for i in range(0, len(inp)-1):
		for j in range(i+1, len(inp)):
			if cmpr(inp[j], inp[i]):
				temp = inp[i]
				inp[i] = inp[j]
				inp[j] = temp
	return inp

if __name__ == '__main__':
	print sort([1,2,5,2,5,4,3])
	mylist = []
	for i in range(10000):
		mylist.append(random.randint(0,10000))
	sort(mylist)


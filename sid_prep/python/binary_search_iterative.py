import math

def search(data, search_for):
	array_size = len(data)
	mid = array_size/2
	while True:
		array_size = array_size / 2
		step_size = int(math.ceil(array_size * 1.0/2)) # prevent step size from reaching zero
		if search_for == data[mid]:
			return mid
		elif search_for < data[mid]:
			mid -= step_size
			if mid < 0:
				mid = 0
		else:
			mid += step_size
			if mid >= len(data):
				mid = len(data)-1
		if array_size == 0:
			return None


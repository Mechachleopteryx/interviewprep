def search(data, search_for):
	mid = len(data)/2
	if len(data) == 0:
		return None
	if search_for == data[mid]:
		return mid
	elif search_for < data[mid]:
		return search(data[:mid], search_for)
	else:
		idx = search(data[mid+1:], search_for)
		if idx is None:
			return idx
		else:
			return mid + 1 + idx


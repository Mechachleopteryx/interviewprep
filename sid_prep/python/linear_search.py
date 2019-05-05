def search(data, search_for):
	for i in range(len(data)):
		if data[i] == search_for:
			return i
	return None


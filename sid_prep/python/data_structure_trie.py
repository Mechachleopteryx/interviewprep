import functools

def ensure_string(func):
	@functools.wraps(func)
	def wrapper(*args, **kwargs):
		elem = len(args) > 0 and args[1] or kwargs['elem']
		if not isinstance(elem, str):
			raise Exception("Element is not a string!")
		return func(*args, **kwargs)
	return wrapper

class Trie(dict):
	@ensure_string
	def insert(self, elem, depth=0):
		""" Inserts the supplied string into the trie. """
		if len(elem) > depth:
			if not self.has_key(elem[depth]):
				self[elem[depth]] = Trie()
			self[elem[depth]].insert(elem, depth+1)
		else:
			if not self.has_key('data'):
				self['data'] = elem
			return None
	
	@ensure_string
	def contains(self, elem, prefix=False, depth=0):
		""" Check if the supplied string is present in the trie,
		either directly or as a prefix. """
		if len(elem) > depth:
			if self.has_key(elem[depth]):
				return self[elem[depth]].contains(elem, prefix, depth+1)
			return False
		else:
			if prefix or self.has_key('data'):
				return True
			return False
	
	@ensure_string
	def delete(self, elem, depth=0):
		""" Deletes a given string from the trie. Note: Does not do garbage
		collection on branches without data. """
		if len(elem) > depth:
			if self.has_key(elem[depth]):
				return self[elem[depth]].delete(elem, depth+1)
			return None
		else:
			if self.has_key('data'):
				return self.pop('data')
			return None


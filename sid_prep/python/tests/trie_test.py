import unittest
import data_structure_trie

class GoodInput(unittest.TestCase):
	def setUp(self):
		self.data = ['abc', 'sky', 'tree', 'hello']
		self.notdata = ['car', 'bear', 'fall', 'cba']
		self.valid_prefixes = ['abc', 'ab', 'a', 'tr', 'hell']
		self.invalid_prefixes = ['ac', 'te', 'b', 'hele']
		self.trie = data_structure_trie.Trie()
		for d in self.data:
			self.trie.insert(d)
	
	def tearDown(self):
		self.trie = None
	
	def test_should_contain_inserted_data(self):
		for d in self.data:
			self.assertTrue(self.trie.contains(d))
	
	def test_should_not_contain_invalid_data(self):
		for d in self.notdata:
			self.assertFalse(self.trie.contains(d))
	
	def test_should_recognize_valid_prefixes(self):
		for p in self.valid_prefixes:
			self.assertTrue(self.trie.contains(p, prefix=True))
	
	def test_should_not_recognize_invalid_prefixes(self):
		for p in self.invalid_prefixes:
			self.assertFalse(self.trie.contains(p, prefix=True))
	
	def test_should_delete_data(self):
		to_delete = ['abc', 'tree']
		for d in to_delete:
			self.trie.delete(d)
		for d in self.data:
			if d in to_delete:
				self.assertFalse(self.trie.contains(d))
			else:
				self.assertTrue(self.trie.contains(d))

class BadInput(unittest.TestCase):
	data = [1,2,3,['abc'],{'abc':1}]
	
	def test_should_raise_error_for_non_string_data(self):
		trie = data_structure_trie.Trie()
		for d in self.data:
			self.assertRaises(Exception, trie.insert, d)

class EdgeCases(unittest.TestCase):
	def setUp(self):
		self.data = ['0', '001', '010', '-ab', 'Ab3', 'Ab4']
		self.notdata = ['01', '00', '--1', '-aa']
		self.valid_prefixes = ['0', '00', '01', '001', '-', 'A', 'Ab']
		self.invalid_prefixes = ['1', '10', '000', 'Ac', '-b']
		self.trie = data_structure_trie.Trie()
		for d in self.data:
			self.trie.insert(d)
	
	def tearDown(self):
		self.trie = None
	
	def test_should_contain_inserted_edge_case_data(self):
		for d in self.data:
			self.assertTrue(self.trie.contains(d))
	
	def test_should_not_contain_invalid_edge_case_data(self):
		for d in self.notdata:
			print '%s: %s' % (d, self.trie.contains(d))
			self.assertFalse(self.trie.contains(d))
	
	def test_should_recognize_valid_edge_case_prefixes(self):
		for p in self.valid_prefixes:
			self.assertTrue(self.trie.contains(p, prefix=True))
	
	def test_should_not_recognize_invalid_edge_case_prefixes(self):
		for p in self.invalid_prefixes:
			self.assertFalse(self.trie.contains(p, prefix=True))

	def test_should_delete_edge_case_data(self):
		to_delete = ['0', '-ab']
		for d in to_delete:
			self.trie.delete(d)
		for d in self.data:
			if d in to_delete:
				self.assertFalse(self.trie.contains(d))
			else:
				self.assertTrue(self.trie.contains(d))



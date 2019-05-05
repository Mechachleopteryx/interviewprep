import unittest
import linear_search
import binary_search
import graph
import bfs
import dfs
import dfs_iterative_deepening

class GoodInput(unittest.TestCase):
	def setUp(self):
		self.search_for = 4
		self.data = [([1,6,14,4,2,8], 3),
				([4,5,9,12,16], 0),
				([10,0,2,15,4], 4)]
		self.notdata = [3, 7, 18]
		# graph-related search
		self.g = graph.Graph()
		self.nodes = [graph.Node('sid'),
				graph.Node('casper'),
				graph.Node('john'),
				graph.Node('meagan'),
				graph.Node('calvin'),
				graph.Node('snowy')]
		for n in self.nodes:
			self.g.add_node(n)
		self.edges = []
		self.edges.append((self.nodes[0], self.nodes[3]))
		self.edges.append((self.nodes[3], self.nodes[1]))
		self.edges.append((self.nodes[0], self.nodes[2]))
		self.edges.append((self.nodes[2], self.nodes[4]))
		self.edges.append((self.nodes[4], self.nodes[1]))
		for n1,n2 in self.edges:
			self.g.add_edge(n1, n2)
		self.shortest_path = ['sid', 'meagan', 'casper']
	
	def test_linear_search_should_find_element(self):
		for d, idx in self.data:
			self.assertEqual(linear_search.search(d, self.search_for), idx)
	
	def test_linear_search_should_not_find_invalid_data(self):
		for search_for in self.notdata:
			for d, idx in self.data:
				self.assertIsNone(linear_search.search(d, search_for))
	
	def test_binary_search_should_find_element(self):
		for d, idx in self.data:
			d = sorted(d)
			self.assertEqual(binary_search.search(d, self.search_for), d.index(self.search_for))
	
	def test_binary_search_should_not_find_invalid_data(self):
		for search_for in self.notdata:
			for d,idx in self.data:
				d = sorted(d)
				self.assertIsNone(binary_search.search(d, search_for))
	
	def test_bfs_should_find_element(self):
		start_node = self.nodes[0]
		self.assertEqual(bfs.search(self.g, 'casper', start_node)[0], self.nodes[1])
	
	def test_bfs_should_find_shortest_path(self):
		start_node = self.nodes[0]
		self.assertEqual(bfs.search(self.g, 'casper', start_node)[1], self.shortest_path)
	
	def test_bfs_should_not_find_invalid_element(self):
		start_node = self.nodes[0]
		self.assertIsNone(bfs.search(self.g, 'borat', start_node))
	
	def test_dfs_should_find_element(self):
		start_node = self.nodes[0]
		self.assertEqual(dfs.search(self.g, 'casper', start_node)[0], self.nodes[1])
	
	def test_dfs_should_not_find_invalid_element(self):
		start_node = self.nodes[0]
		self.assertIsNone(dfs.search(self.g, 'borat', start_node))

	def test_dfs_iterative_deepening_should_find_element(self):
		start_node = self.nodes[0]
		self.assertEqual(dfs_iterative_deepening.search(self.g, 'casper', start_node)[0], self.nodes[1])
	
	def test_dfs_iterative_deepening_should_find_shortest_path(self):
		start_node = self.nodes[0]
		self.assertEqual(dfs_iterative_deepening.search(self.g, 'casper', start_node)[1], self.shortest_path)
	
	def test_dfs_iterative_deepening_should_not_find_invalid_element(self):
		start_node = self.nodes[0]
		self.assertIsNone(dfs_iterative_deepening.search(self.g, 'borat', start_node))
	
class BadInput(unittest.TestCase):
	pass

class EdgeCases(unittest.TestCase):
	search_for = 4
	data = [([4,], [0,]),
			([4,4,4], [0,1,2]),
			([1,4,4,3],[1,2])]
	notdata = [0, 5, 18]
	
	def test_linear_search_should_find_element(self):
		for d, idxs in self.data:
			self.assertIn(linear_search.search(d, self.search_for), idxs)
	
	def test_linear_search_should_not_find_invalid_data(self):
		for search_for in self.notdata:
			for d, idxs in self.data:
				self.assertIsNone(linear_search.search(d, search_for))
	
	def test_binary_search_should_find_element(self):
		for d, idxs in self.data:
			d = sorted(d)
			self.assertIn(binary_search.search(d, self.search_for), idxs)
	
	def test_binary_search_should_not_find_invalid_data(self):
		for search_for in self.notdata:
			for d,idxs in self.data:
				d = sorted(d)
				self.assertIsNone(binary_search.search(d, search_for))


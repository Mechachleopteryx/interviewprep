import unittest
import graph

class GoodInput(unittest.TestCase):
	def setUp(self):
		self.nodes = [graph.Node('sid'),
			graph.Node('meagan'),
			graph.Node('casper'),
			graph.Node('john')]
		self.not_present = ['jack', 'jill', 'shantanu']
		self.g = graph.Graph()
		for node in self.nodes:
			self.g.add_node(node)
		self.edges = []
		self.edges.append((self.nodes[0], self.nodes[1]))
		self.edges.append((self.nodes[1], self.nodes[2]))
		for n1,n2 in self.edges:
			self.g.add_edge(n1, n2)
	
	def test_graph_contains_added_nodes(self):
		for node in self.nodes:
			self.assertTrue(self.g.get_node(name=node.name))
	
	def test_graph_does_not_contain_invalid_nodes(self):
		for name in self.not_present:
			self.assertIsNone(self.g.get_node(name))
	
	def test_graph_contains_added_edges(self):
		for n1, n2 in self.edges:
			self.assertTrue(n2 in n1.neighbors)
			self.assertTrue(n1 in n2.neighbors)

class BadInput(unittest.TestCase):
	pass

class EdgeCases(unittest.TestCase):
	pass


# adjacency list implementation
class Node(object):
	def __init__(self, name):
		self.name = name
		self.neighbors = []
	
	def __str__(self):
		return str(self.name)

	def __repr__(self):
		return 'Node(name=%r)' % (self.name)

class Graph(object):
	def __init__(self):
		self.node_index = {}
	
	def add_node(self, nn):
		self.node_index[nn.name] = nn
	
	def get_node(self, name):
		if not name in self.node_index:
			return None
		return self.node_index[name]
	
	def add_edge(self, n1, n2):
		if not n2 in n1.neighbors:
			n1.neighbors.append(n2)
		if not n1 in n2.neighbors:
			n2.neighbors.append(n1)


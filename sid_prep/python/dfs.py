def search(graph, goal_id, start_node):
	visited = {start_node.name: None}
	def _search(cur_node):
		if cur_node.name == goal_id:
			return (cur_node, [cur_node.name])
		unvisited_neighbors = filter(lambda f: f.name not in visited, cur_node.neighbors)
		for n in unvisited_neighbors:
			visited[n.name] = cur_node.name
			res = _search(n)
			if res:
				return (res[0], [cur_node.name] + res[1]) # result as well as path taken
		return None
	return _search(start_node)


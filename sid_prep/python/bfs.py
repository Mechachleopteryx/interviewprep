def search(graph, goal_id, start_node):
	visited = {start_node.name: None}
	unvisited_nodes = []
	unvisited_nodes.append(start_node)
	nodequeue = iter(unvisited_nodes)
	for n in nodequeue:
		if n.name == goal_id:
			break
		else:
			unvisited_neighbors = filter(lambda f: f.name not in visited, n.neighbors)
			for un in unvisited_neighbors:
				visited[un.name] = n.name
			unvisited_nodes.extend(unvisited_neighbors)
	if n.name == goal_id:
		# retrace path
		cur = goal_id
		path = [cur]
		while cur != start_node.name:
			cur = visited[cur]
			path.append(cur)
		path = list(reversed(path))
		return (n, path)
	return None


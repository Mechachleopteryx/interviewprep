MAX_DEPTH_REACHED = -1

def search(graph, goal_id, start_node):
	def _search(cur_node, cur_depth):
		if cur_node.name == goal_id:
			return (cur_node, [cur_node.name])
		elif cur_depth > up_to_depth:
			return MAX_DEPTH_REACHED
		unvisited_neighbors = filter(lambda f: f.name not in visited or f.name in visited and visited[f.name] > cur_depth+1, cur_node.neighbors) # explore neighbor only if current path to it is shortest so far
		max_depth_reached_on_path = False
		for n in unvisited_neighbors:
			visited[n.name] = cur_depth + 1 # records length of shortest path to node found so far
			res = _search(n, cur_depth+1)
			if res:
				if res == MAX_DEPTH_REACHED:
					max_depth_reached_on_path = True # record that at least one path hit the depth limit before being fully explored
					continue
				return (res[0], [cur_node.name] + res[1]) # result as well as path taken
		if max_depth_reached_on_path:
			return MAX_DEPTH_REACHED
		return None
	
	up_to_depth = 0
	while True:
		visited = {start_node.name: 0}
		res = _search(start_node, 0)
		if res:
			if res == MAX_DEPTH_REACHED:
				up_to_depth += 1 # keep increasing depth as long as there are paths that have not been fully explored
			else:
				break
		else:
			break
	return res


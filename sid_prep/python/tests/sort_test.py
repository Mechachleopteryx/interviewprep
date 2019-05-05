import unittest
import selection_sort
import insertion_sort
import bubble_sort
import merge_sort

class GoodInput(unittest.TestCase):
	i_o = []

	def setUp(self):
		self.i_o = [([1,5,3,2,4], [1,2,3,4,5]),
			([1,2,3,4,5], [1,2,3,4,5]),
			([4,1,2,3,5], [1,2,3,4,5]),
			([-1,1], [-1,1]),
			([1,-1], [-1,1]),
			([-1,-2,-3,-4,-5,1,2,3,4,5], [-5,-4,-3,-2,-1,1,2,3,4,5])]

	def test_selectionsort_should_sort_lists_correctly(self):
		for k,v in self.i_o:
			self.assertEqual(selection_sort.sort(k), v)

	def test_insertionsort_should_sort_lists_correctly(self):
		for k,v in self.i_o:
			self.assertEqual(insertion_sort.sort(k), v)

	def test_bubblesort_should_sort_lists_correctly(self):
		for k,v in self.i_o:
			self.assertEqual(bubble_sort.sort(k), v)

	def test_mergesort_should_sort_lists_correctly(self):
		for k,v in self.i_o:
			self.assertEqual(merge_sort.sort(k), v)

class BadInput(unittest.TestCase):
	pass

class EdgeCases(unittest.TestCase):
	i_o = []

	def setUp(self):
		self.i_o = [([0,0,0], [0,0,0]),
			([-1,-1,-1], [-1,-1,-1]),
			([], []),
			([0,0,-1], [-1,0,0]),
			([0,1,0], [0,0,1]),
			([3,4,3,2,1,2,5,1,2], [1,1,2,2,2,3,3,4,5])]

	def test_selectionsort_should_handle_edge_cases(self):
		for k,v in self.i_o:
			self.assertEqual(selection_sort.sort(k), v)

	def test_insertionsort_should_handle_edge_cases(self):
		for k,v in self.i_o:
			self.assertEqual(insertion_sort.sort(k), v)

	def test_bubblesort_should_handle_edge_cases(self):
		for k,v in self.i_o:
			self.assertEqual(bubble_sort.sort(k), v)

	def test_mergesort_should_handle_edge_cases(self):
		for k,v in self.i_o:
			self.assertEqual(merge_sort.sort(k), v)


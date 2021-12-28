# frozen_string_literal: true

# Simple priority queue based on a sorted array and using Array#bsearch_index
# to efficiently insert new items into the array. One thing that would be
# interesting would be to see how efficient this is compared to a heap. But Ruby
# does not have a heap structure in the standard library as far as I am aware
# and this is much simpler for a little program like this.
class PriorityQueue
  def initialize(list = [], value_function = :itself)
    @list = list.sort_by(&value_function)
    @value_function = value_function.to_proc
  end

  def insert(element)
    index = @list.bsearch_index { |e| @value_function.call(e) > @value_function.call(element) } || @list.length
    @list.insert(index, element)
  end

  def pop
    @list.shift
  end

  def empty?
    @list.empty?
  end
end

# frozen_string_literal: true

require 'passage_pathing/cave_map'
require 'passage_pathing/node'
require 'passage_pathing/path_validation'

# Methods to solve day 12. Most functionality is in other classes/modules
# within this namespace. This could use some optimization/cleanup
module PassagePathing
  def self.parse_input(io)
    io.each_line.each_with_object(CaveMap.new) do |line, graph|
      node_a, node_b = parse_line(line.chomp)
      graph.add_node(node_a)
      graph.add_node(node_b)
      graph.add_edge(node_a, node_b)
    end
  end

  def self.parse_line(line)
    labels = line.split('-')
    labels.map { |label| Node.new(label) }
  end
end

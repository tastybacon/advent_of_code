# frozen_string_literal: true

module PassagePathing
  # Graph-style representation of a cave system
  class CaveMap
    attr_reader :nodes, :edges

    def initialize
      @nodes = {}
      @edges = {}
    end

    def add_node(node)
      @nodes[node.label] = node
    end

    def add_edge(node_a, node_b)
      @edges[node_a.label] ||= []
      @edges[node_a.label] << node_b
      @edges[node_b.label] ||= []
      @edges[node_b.label] << node_a
    end

    def get_all_paths(node_validator)
      paths = []
      initial_path = [nodes['start']]
      travel(paths, node_validator, initial_path, Hash.new(0))
      paths
    end

    def travel(path_list, node_validator, current_path, small_caves_visited)
      current_node = current_path.last
      if current_node.type == :end
        path_list << current_path
        return
      end
      edges[current_node.label].each do |node|
        visit_node(path_list, node_validator, node, current_path, small_caves_visited)
      end
    end

    def visit_node(path_list, node_validator, node, current_path, small_caves_visited)
      return unless node_validator.call(node, small_caves_visited)

      new_small_caves = {}
      new_small_caves[node.label] = 1 if node.type == :small_cave

      travel(
        path_list,
        node_validator,
        current_path + [node],
        small_caves_visited.merge(new_small_caves) { |_k, v1, v2| v1 + v2 }
      )
    end
  end
end

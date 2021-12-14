# frozen_string_literal: true

module PassagePathing
  # Methods to determine if the next node in a path is valid
  module PathValidation
    def self.get_node_validator(path_strategy)
      lambda do |node, small_caves_visited|
        case node.type
        when :start
          false
        when :large_cave, :end
          true
        when :small_cave
          validate_next_small_cave(node, small_caves_visited, path_strategy)
        end
      end
    end

    def self.validate_next_small_cave(next_small_cave, small_caves_visited, path_strategy)
      case path_strategy
      when :small_caves_once
        !small_caves_visited.key?(next_small_cave.label)
      when :at_most_one_small_cave_twice
        one_small_cave_visited_twice?(next_small_cave, small_caves_visited)
      end
    end

    def self.one_small_cave_visited_twice?(next_small_cave, small_caves_visited)
      case small_caves_visited[next_small_cave.label]
      when 2
        false
      when 1
        small_caves_visited.none? { |_label, visit_count| visit_count > 1 }
      else
        true
      end
    end
  end
end

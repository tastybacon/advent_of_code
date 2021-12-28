# frozen_string_literal: true

require 'util/priority_queue'

require 'chiton/map'

# Methods to solve day 15, finding the path through a cave full of chitons
module Chiton
  def self.parse_input(io)
    io.each_line.map { |line| line.chomp.chars.map(&:to_i) }
  end

  def self.find_least_risky_path(map) # rubocop:disable Metrics/AbcSize, Metrics/MethodLength
    start = map.top_left
    goal = map.bottom_right
    came_from = { start => nil }

    gscore = { start => 0 }
    fscore = { start => gscore[start] + manhattan_distance(start, goal) }

    new_points = PriorityQueue.new([start], ->(element) { fscore[element] })

    until new_points.empty?
      current = new_points.pop
      return reconstruct_path(came_from, current) if current == goal

      neighbor_coordinates(current[0], current[1]).each do |neighbor|
        neighbor_risk = map.get_risk_level(neighbor)
        next if neighbor_risk.nil?

        tentative_gscore = gscore[current] + neighbor_risk

        next unless gscore[neighbor].nil? || tentative_gscore < gscore[neighbor]

        came_from[neighbor] = current
        gscore[neighbor] = tentative_gscore
        fscore[neighbor] = tentative_gscore + manhattan_distance(neighbor, goal)
        new_points.insert(neighbor)
      end
    end
  end

  def self.get_risk_level_of_path(map, path)
    # The risk of the starting point is not counted
    path[1..].sum { |x| map.get_risk_level(x) }
  end

  def self.reconstruct_path(came_from, goal)
    node = goal
    path = []
    until node.nil?
      path << node
      node = came_from[node]
    end
    path.reverse!
  end

  def self.neighbor_coordinates(row, column)
    [
      [row + 1, column],
      ([row - 1, column] if row.positive?),
      [row, column + 1],
      ([row, column - 1] if column.positive?),
    ].compact
  end

  def self.manhattan_distance(point, goal)
    (point[0] - goal[0]).abs + (point[1] - goal[1]).abs
  end
end

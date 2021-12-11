# frozen_string_literal: true

require 'set'

# Module for day 9, identifying risks and basins of ocean floor height maps
module SmokeBasin
  MAX_HEIGHT = 9

  def self.parse_input(io)
    io.each_line.map { |line| line.chomp.chars.map(&:to_i) }
  end

  def self.get_low_points(height_map)
    height_map.each_with_object([]).each_with_index do |(row, acc), row_index|
      row.each_with_index do |_value, column_index|
        acc << [row_index, column_index] if low_point?(height_map, row_index, column_index)
      end
    end
  end

  def self.risk_levels(height_map, coordinates)
    coordinates.each_with_object({}) do |point, acc|
      row, col = point
      acc[point] = risk_level(height_map, row, col)
    end
  end

  def self.risk_level(height_map, row_index, column_index)
    height_map[row_index][column_index] + 1
  end

  def self.neighbor_coordinates(row, column)
    [
      [row + 1, column],
      ([row - 1, column] if row.positive?),
      [row, column + 1],
      ([row, column - 1] if column.positive?),
    ].compact
  end

  def self.neighbor_values(height_map, row, column)
    neighbor_coordinates(row, column).map { |(x, y)| height_map.dig(x, y) }.compact
  end

  def self.low_point?(height_map, row, column)
    value = height_map[row][column]
    neighbor_values(height_map, row, column).all? { |v| v > value }
  end

  def self.find_basins(height_map)
    height_map = height_map.map(&:dup)
    height_map.each_with_object([]).each_with_index do |(row, acc), row_index|
      row.each_with_index do |value, column_index|
        next if value.nil?

        basin = find_basin_at(height_map, row_index, column_index)
        next if basin.nil? || basin.empty?

        acc << basin
      end
    end
  end

  def self.find_basin_at(height_map, starting_row, starting_column) # rubocop:disable Metrics/AbcSize, Metrics/CyclomaticComplexity, Metrics/MethodLength
    starting_value = height_map[starting_row, starting_column]
    return nil if starting_value.nil? || starting_value == MAX_HEIGHT

    points_seen = Set.new
    points_in_basin = Set.new

    queue = Queue.new
    queue << [starting_row, starting_column]
    until queue.empty?
      point = queue.pop
      next if points_seen.include?(point)

      points_seen << point
      row, column = point
      current_value = height_map.dig(row, column)
      next if current_value.nil? || current_value == MAX_HEIGHT

      height_map[row][column] = nil

      points_in_basin << point
      neighbor_coordinates(row, column).each { |neighbor| queue.push(neighbor) }
    end

    points_in_basin
  end
end

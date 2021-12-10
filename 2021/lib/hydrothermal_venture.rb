# frozen_string_literal: true

# Methods to solve day 5. Can be used to determine the danger posed to the
#   submarine caused by hydrothermal vents on the ocean floor
module HydrothermalVenture
  def self.parse_input(input)
    input.each_line.map do |line|
      results = /^(\d+),(\d+) -> (\d+),(\d+)$/.match(line)
      VentLine.new(
        results[1].to_i,
        results[2].to_i,
        results[3].to_i,
        results[4].to_i
      )
    end
  end

  def self.get_dangerous_points(lines)
    lines
      .flat_map { |line| line.points.map(&:hash) }
      .tally
      .select! { |_point, danger_level| danger_level > 1 }
      .map(&:first)
  end

  # Represents a hydrothermal vent on the ocean floor. Based on the given
  # coordinates, the `#points` method can list all the points affected by this
  #   vent.
  # The given coordinates should be a part of a horizontal, vertical, or 45
  #   degree diagonal line.
  class VentLine
    def initialize(x1, y1, x2, y2)
      @x1 = x1
      @y1 = y1
      @x2 = x2
      @y2 = y2
    end

    def horizontal?
      @y1 == @y2
    end

    def vertical?
      @x1 == @x2
    end

    def points
      if vertical?
        Range.new(*[@y1, @y2].minmax).map { |y| [@x1, y] }
      else
        # If the line is not horizontal or vertical, it is always diagonal
        #   (i.e. the slope is 1 or -1)
        slope = (@y1 - @y2) / (@x1 - @x2)
        (min_x, starting_y), (max_x,) = [[@x1, @y1], [@x2, @y2]].minmax
        (min_x..max_x).each_with_index.map { |x, index| [x, starting_y + (index * slope)] }
      end
    end
  end
end

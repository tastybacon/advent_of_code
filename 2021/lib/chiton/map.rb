# frozen_string_literal: true

module Chiton
  # Represents a map of a cave. `map` is a square 2D array with integer risk
  # levels from 1-9.
  class Map
    # Assumes `map` is square. `expansion` multiplies the dimensions of the map
    # and derives the risk levels for the expanded sections according to the
    # problem description.
    def initialize(map, expansion = 1)
      @map = map
      @expansion = expansion
    end

    def top_left
      [0, 0]
    end

    def bottom_right
      [max_x, max_y]
    end

    def max_x
      (@map.length * @expansion) - 1
    end

    def max_y
      (@map.length * @expansion) - 1
    end

    def get_risk_level(point)
      return nil if point[1] > max_x || point[0] > max_y

      base_risk = get_base_risk(point)

      extra_risk_x = point[1] / @map.length
      extra_risk_y = point[0] / @map.length
      clamp_risk(base_risk + extra_risk_x + extra_risk_y)
    end

    def get_base_risk(point)
      base_coord_x = point[1] % @map.length
      base_coord_y = point[0] % @map.length
      @map.dig(base_coord_x, base_coord_y)
    end

    def clamp_risk(calculated_risk)
      ((calculated_risk - 1) % 9) + 1
    end
  end
end

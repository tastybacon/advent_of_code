# frozen_string_literal: true

module Dive
  # Follows submarine instructions. Each instruction should consist of a
  #   direction and a distance.
  # `up` will decrease the submarine's depth by the given distance.
  # `down` will increase the submarine's depth by the given distance.
  # `forward` will move `horizontal_position` by the given distance.
  class SimpleInstructionExecutor
    attr_reader :horizontal_position, :depth

    def initialize(horizontal_position: 0, depth: 0)
      self.horizontal_position = horizontal_position
      self.depth = depth
    end

    def follow_instructions(instructions)
      instructions.each do |(direction, distance)|
        follow_instruction(direction, distance)
      end
    end

    def follow_instruction(direction, distance)
      case direction
      when 'forward'
        self.horizontal_position += distance
      when 'up'
        self.depth -= distance
      when 'down'
        self.depth += distance
      end
    end

    private

    attr_writer :horizontal_position, :depth, :aim
  end
end

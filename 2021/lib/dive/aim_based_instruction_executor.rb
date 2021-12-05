# frozen_string_literal: true

module Dive
  # Follows submarine instructions. Each instruction should consist of a
  #   direction and a delta.
  # `up` will decrease the submarine's aim by the given delta.
  # `down` will increase the submarine's aim by the given delta.
  # `forward` will move `horizontal_position` by the given delta and change
  #   the depth by the given delta multiplied by the current aim.
  class AimBasedInstructionExecutor
    attr_reader :horizontal_position, :depth, :aim

    def initialize(horizontal_position: 0, depth: 0, aim: 0)
      self.horizontal_position = horizontal_position
      self.depth = depth
      self.aim = aim
    end

    def follow_instructions(instructions)
      instructions.each do |(direction, delta)|
        follow_instruction(direction, delta)
      end
    end

    def follow_instruction(direction, delta)
      case direction
      when 'forward'
        self.horizontal_position += delta
        self.depth += delta * aim
      when 'up'
        self.aim -= delta
      when 'down'
        self.aim += delta
      end
    end

    private

    attr_writer :horizontal_position, :depth, :aim
  end
end

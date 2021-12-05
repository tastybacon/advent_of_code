# frozen_string_literal: true

require 'set'

require 'dive/simple_instruction_executor'
require 'dive/aim_based_instruction_executor'

# Methods to solve day 2
module Dive
  def self.parse_instructions(io)
    io.each_line.map { |line| parse_instruction(line.chomp) }
  end

  def self.parse_instruction(instruction_string)
    direction, distance = instruction_string.split
    [direction, distance.to_i]
  end
end

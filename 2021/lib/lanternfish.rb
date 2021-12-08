# frozen_string_literal: true

# Methods to solve day 6, estimating the population of lanternfish after a
# given number of days.
module Lanternfish
  # The expected input is a string of comma separated integers. The maximum
  # of a single value should be 6
  def self.parse_input(io)
    io.split(',').map(&:to_i)
  end

  # Based on the input fish and the number of days, estimate the population of
  # fish after the number of days.
  def self.estimate_population(fish, days)
    simulate_steps(fish, days).sum
  end

  # This method takes an array where each element is the number of days until
  # a single fish spawns and returns an array where the index of the element
  # represents the number of days to spawn and the element at that index
  # represents the number of fish.
  def self.collapse_fish(input_fish)
    input_fish.each_with_object(Array.new(9, 0)) do |days_to_spawn, acc|
      acc[days_to_spawn] += 1
    end
  end

  def self.simulate_steps(fish, days)
    fish_summary = collapse_fish(fish)
    days.times do
      number_of_spawns = fish_summary[0]
      fish_summary.rotate!
      fish_summary[6] += number_of_spawns
      fish_summary[8] = number_of_spawns
    end
    fish_summary
  end
end

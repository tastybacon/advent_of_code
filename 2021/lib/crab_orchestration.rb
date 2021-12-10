# frozen_string_literal: true

# Methods to solve day 7. Finding the most fuel-efficient way of lining up
# mini submarines piloted by crabs.
module CrabOrchestration
  def self.parse_input(io)
    io.split(',').map(&:to_i)
  end

  def self.fuel_usage(position, destination, fuel_consumption_strategy)
    case fuel_consumption_strategy
    when :simple
      (position - destination).abs
    when :accumulating
      diff = (position - destination).abs
      ((diff / 2.to_f) * (1 + diff)).to_i
    else
      raise "Unknown fuel consumption strategy: '#{fuel_consumption_strategy}'"
    end
  end

  def self.fuel_usage_multiple(position_counts, destination, fuel_consumption_model)
    position_counts.sum do |position, count|
      fuel_usage(position, destination, fuel_consumption_model) * count
    end
  end

  def self.determine_most_fuel_efficient_position(crab_positions, fuel_consumption_model)
    position_counts = crab_positions.tally
    last_fuel_usage = nil
    last_position = nil
    iterate_positions(crab_positions, position_counts, fuel_consumption_model).each do |destination|
      current_fuel_usage = fuel_usage_multiple(position_counts, destination, fuel_consumption_model)
      return [last_position, last_fuel_usage] if !last_fuel_usage.nil? && current_fuel_usage >= last_fuel_usage

      last_position = destination
      last_fuel_usage = current_fuel_usage
    end
  end

  # Iterates over different values in a semi-smart way. First, we guess that
  # the most efficient position will be somewhere towards the middle and so
  # we start at the average. Then we determine the direction of iteration based
  # on two values.
  def self.iterate_positions(crab_positions, position_counts, fuel_consumption_model)
    guess = position_counts.sum { |a, b| a * b } / crab_positions.length
    guess_fuel = position_counts.sum { |p, c| fuel_usage(p, guess, fuel_consumption_model) * c }
    previous_fuel = position_counts.sum { |p, c| fuel_usage(p, guess - 1, fuel_consumption_model) * c }
    if guess_fuel > previous_fuel
      (guess - 1..).step(-1)
    else
      (guess..)
    end
  end
end

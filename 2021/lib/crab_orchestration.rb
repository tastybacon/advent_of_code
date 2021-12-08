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

  def self.determine_most_fuel_efficient_position(crab_positions, fuel_consumption_model)
    min_position, max_position = crab_positions.minmax
    last_fuel_usage = nil
    last_position = min_position
    (min_position..max_position).each do |destination|
      current_fuel_usage = crab_positions.sum { |p| fuel_usage(p, destination, fuel_consumption_model) }
      return [last_position, last_fuel_usage] if !last_fuel_usage.nil? && current_fuel_usage >= last_fuel_usage

      last_position = destination
      last_fuel_usage = current_fuel_usage
    end
  end
end

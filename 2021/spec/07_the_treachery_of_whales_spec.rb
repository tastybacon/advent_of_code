# frozen_string_literal: true

require 'crab_orchestration'

describe 'Day 07: The Treachery of Whales' do
  context 'Part 1' do
    it 'estimates the amount of fuel needed for the test data' do
      input = CrabOrchestration.parse_input(File.read('resources/07/test_input.txt'))
      position, fuel_cost = CrabOrchestration.determine_most_fuel_efficient_position(input, :simple)
      expect(position).to eq 2
      expect(fuel_cost).to eq 37
    end

    it 'determines the most efficient position of crabs' do
      input = CrabOrchestration.parse_input(File.read('resources/07/input.txt'))
      _, fuel_cost = CrabOrchestration.determine_most_fuel_efficient_position(input, :simple)
      expect(fuel_cost).to eq 337_488
    end
  end

  context 'Part 2' do
    it 'estimates the amount of fuel needed for the test data' do
      input = CrabOrchestration.parse_input(File.read('resources/07/test_input.txt'))
      position, fuel_cost = CrabOrchestration.determine_most_fuel_efficient_position(input, :accumulating)
      expect(position).to eq 5
      expect(fuel_cost).to eq 168
    end

    it 'determines the most efficient position of crabs' do
      input = CrabOrchestration.parse_input(File.read('resources/07/input.txt'))
      _, fuel_cost = CrabOrchestration.determine_most_fuel_efficient_position(input, :accumulating)
      expect(fuel_cost).to eq 89_647_695
    end
  end
end

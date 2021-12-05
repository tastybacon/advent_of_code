# frozen_string_literal: true

require 'diagnostic_interpreter'

describe 'Day 03: Binary Diagnostic' do
  context 'Part 1' do
    it 'calculates the correct values for the test input' do
      input = File.readlines('resources/03/test_input.txt').map(&:chomp)
      result = DiagnosticInterpreter.new(input)
      expect(result.gamma_rate).to eq 22
      expect(result.epsilon_rate).to eq 9
      expect(result.power_consumption).to eq 198
    end

    it 'calculates the correct values' do
      input = File.readlines('resources/03/input.txt').map(&:chomp)
      result = DiagnosticInterpreter.new(input)
      expect(result.power_consumption).to eq 4_147_524
    end
  end

  context 'Part 2' do
    it 'calculates the correct values for the test input' do
      input = File.readlines('resources/03/test_input.txt').map(&:chomp)
      result = DiagnosticInterpreter.new(input)
      expect(result.oxygen_generator_rating).to eq 23
      expect(result.c02_scrubber_rating).to eq 10
      expect(result.life_support_rating).to eq 230
    end

    it 'calculates the correct values' do
      input = File.readlines('resources/03/input.txt').map(&:chomp)
      result = DiagnosticInterpreter.new(input)
      expect(result.life_support_rating).to eq 3_570_354
    end
  end
end

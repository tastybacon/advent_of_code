# frozen_string_literal: true

require 'sonar_sweep'

describe 'Day 1: Sonar Sweep' do
  context 'Part 1' do
    it 'sums the test data correctly' do
      input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      expect(SonarSweep.count_increments(input)).to eq 7
    end

    it 'sums the problem data correctly' do
      input = File.readlines('resources/01/input_data.txt').map { |line| line.chomp.to_i }
      expect(SonarSweep.count_increments(input)).to eq 1766
    end
  end

  context 'Part 2' do
    it 'sums the test data correctly' do
      input = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]
      expect(SonarSweep.count_increments_with_window(input)).to eq 5
    end

    it 'sums the problem data correctly' do
      input = File.readlines('resources/01/input_data.txt').map { |line| line.chomp.to_i }
      expect(SonarSweep.count_increments_with_window(input)).to eq 1797
    end
  end
end

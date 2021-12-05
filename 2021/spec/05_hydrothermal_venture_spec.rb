# frozen_string_literal: true

require 'hydrothermal_venture'

describe 'Day 04: Giant Squid' do
  context 'Part 1' do
    it 'correctly determines the number of dangerous points in the test data' do
      input = File.read('resources/05/test_input.txt')
      lines = HydrothermalVenture.parse_input(input)
      lines = lines.select { |l| l.horizontal? || l.vertical? }
      result = HydrothermalVenture.get_dangerous_points(lines)
      expect(result.count).to eq 5
    end

    it 'correctly determines the number of dangerous points in the actual data' do
      input = File.read('resources/05/input.txt')
      lines = HydrothermalVenture.parse_input(input)
      lines = lines.select { |l| l.horizontal? || l.vertical? }
      result = HydrothermalVenture.get_dangerous_points(lines)
      expect(result.count).to eq 5147
    end
  end

  context 'Part 2' do
    it 'correctly determines the number of dangerous points in the test data' do
      input = File.read('resources/05/test_input.txt')
      lines = HydrothermalVenture.parse_input(input)
      result = HydrothermalVenture.get_dangerous_points(lines)
      expect(result.count).to eq 12
    end

    it 'correctly determines the number of dangerous points in the actual data' do
      input = File.read('resources/05/input.txt')
      lines = HydrothermalVenture.parse_input(input)
      result = HydrothermalVenture.get_dangerous_points(lines)
      expect(result.count).to eq 16_925
    end
  end
end

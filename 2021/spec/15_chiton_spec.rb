# frozen_string_literal: true

require 'chiton'

describe 'Day 15: Chiton' do
  let(:test_input) { Chiton.parse_input(File.read('resources/15/test_input.txt')) }
  let(:input) { Chiton.parse_input(File.read('resources/15/input.txt')) }
  context 'Part 1' do
    it 'finds a path with the least risk through the test data' do
      map = Chiton::Map.new(test_input)
      path = Chiton.find_least_risky_path(map)
      total_risk = Chiton.get_risk_level_of_path(map, path)
      expect(total_risk).to eq 40
    end

    it 'finds a path with the least risk' do
      map = Chiton::Map.new(input)
      path = Chiton.find_least_risky_path(map)
      total_risk = Chiton.get_risk_level_of_path(map, path)
      expect(total_risk).to eq 652
    end
  end

  context 'Part 2' do
    it 'finds a path with the least risk through the expanded test data' do
      map = Chiton::Map.new(test_input, 5)
      path = Chiton.find_least_risky_path(map)
      total_risk = Chiton.get_risk_level_of_path(map, path)
      expect(total_risk).to eq 315
    end

    it 'finds a path with the least risk through the expanded data' do
      map = Chiton::Map.new(input, 5)
      path = Chiton.find_least_risky_path(map)
      total_risk = Chiton.get_risk_level_of_path(map, path)
      expect(total_risk).to eq 2938
    end
  end
end

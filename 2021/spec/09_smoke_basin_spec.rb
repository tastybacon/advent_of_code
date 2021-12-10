# frozen_string_literal: true

require 'smoke_basin'

describe 'Day 9: Smoke Basin' do
  context 'Part 1' do
    it 'identifies the risk level of the test height map' do
      height_map = SmokeBasin.parse_input(File.read('resources/09/test_input.txt'))
      low_points = SmokeBasin.get_low_points(height_map)
      expect(SmokeBasin.risk_levels(height_map, low_points).values.sum).to eq 15
    end

    it 'identifies the risk level of a height map' do
      height_map = SmokeBasin.parse_input(File.read('resources/09/input.txt'))
      low_points = SmokeBasin.get_low_points(height_map)
      expect(SmokeBasin.risk_levels(height_map, low_points).values.sum).to eq 537
    end
  end

  context 'Part 2' do
    it 'finds all basins in the test height map' do
      height_map = SmokeBasin.parse_input(File.read('resources/09/test_input.txt'))
      basins = SmokeBasin.find_basins(height_map)
      expect(basins.map(&:length).sort.reverse.first(3).reduce(1, :*)).to eq 1134
    end

    it 'finds all basins in a height map' do
      height_map = SmokeBasin.parse_input(File.read('resources/09/input.txt'))
      basins = SmokeBasin.find_basins(height_map)
      expect(basins.map(&:length).sort.reverse.first(3).reduce(1, :*)).to eq 1_142_757
    end
  end
end

# frozen_string_literal: true

require 'lanternfish'

describe 'Day 06: Lanternfish' do
  context 'Part 1' do
    it 'estimates the test population of lanternfish after 80 days' do
      fish = Lanternfish.parse_input(File.read('resources/06/test_input.txt'))
      population = Lanternfish.estimate_population(fish, 18)
      expect(population).to eq 26
      population = Lanternfish.estimate_population(fish, 80)
      expect(population).to eq 5934
    end

    it 'estimates the population of lanternfish after 80 days' do
      fish = Lanternfish.parse_input(File.read('resources/06/input.txt'))
      population = Lanternfish.estimate_population(fish, 80)
      expect(population).to eq 353_274
    end
  end

  context 'Part 2' do
    it 'estimates the test population of lanternfish after 256 days' do
      fish = Lanternfish.parse_input(File.read('resources/06/test_input.txt'))
      population = Lanternfish.estimate_population(fish, 256)
      expect(population).to eq 26_984_457_539
    end

    it 'estimates the population of lanternfish after 256 days' do
      fish = Lanternfish.parse_input(File.read('resources/06/input.txt'))
      population = Lanternfish.estimate_population(fish, 256)
      expect(population).to eq 1_609_314_870_967
    end
  end
end

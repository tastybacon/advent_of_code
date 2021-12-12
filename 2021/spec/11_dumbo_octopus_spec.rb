# frozen_string_literal: true

require 'dumbo_octopus'

describe 'Day 11: Dumbo Octopus' do
  let(:example_octopi) { DumboOctopus.parse_input(File.read('resources/11/test_input.txt')) }
  let(:octopi) { DumboOctopus.parse_input(File.read('resources/11/input.txt')) }

  context 'Part 1' do
    it 'counts the number of flashes after 100 steps with the test input' do
      flash_count = DumboOctopus.simulate(example_octopi).take(100).sum
      expect(flash_count).to eq 1656
    end

    it 'counts the number of flashes after 100 steps with the actual input' do
      flash_count = DumboOctopus.simulate(octopi).take(100).sum
      expect(flash_count).to eq 1721
    end
  end

  context 'Part 2' do
    it 'finds the first step where all octopi flash simultaneously with the test data' do
      step_number = DumboOctopus.find_bright_step(example_octopi)
      expect(step_number).to eq 195
    end

    it 'finds the first step where all octopi flash simultaneously' do
      step_number = DumboOctopus.find_bright_step(octopi)
      expect(step_number).to eq 298
    end
  end
end

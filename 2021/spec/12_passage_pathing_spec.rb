# frozen_string_literal: true

require 'passage_pathing'

describe 'Day 12: Passage Pathing' do
  let(:test_map_one) { PassagePathing.parse_input(File.read('resources/12/test_input.txt')) }
  let(:test_map_two) { PassagePathing.parse_input(File.read('resources/12/test_input_2.txt')) }
  let(:test_map_three) { PassagePathing.parse_input(File.read('resources/12/test_input_3.txt')) }
  let(:map) { PassagePathing.parse_input(File.read('resources/12/input.txt')) }

  context 'Part 1' do
    let(:node_validator) { PassagePathing::PathValidation.get_node_validator(:small_caves_once) }

    it 'counts the correct number of paths through the test data' do
      expect(test_map_one.get_all_paths(node_validator).count).to eq 10
      expect(test_map_two.get_all_paths(node_validator).count).to eq 19
      expect(test_map_three.get_all_paths(node_validator).count).to eq 226
    end

    it 'counts the correct number of paths' do
      expect(map.get_all_paths(node_validator).count).to eq 4691
    end
  end

  context 'Part 2' do
    let(:node_validator) { PassagePathing::PathValidation.get_node_validator(:at_most_one_small_cave_twice) }

    it 'counts the correct number of paths through the test data' do
      expect(test_map_one.get_all_paths(node_validator).count).to eq 36
      expect(test_map_two.get_all_paths(node_validator).count).to eq 103
      expect(test_map_three.get_all_paths(node_validator).count).to eq 3509
    end

    it 'counts the correct number of paths' do
      expect(map.get_all_paths(node_validator).count).to eq 140_718
    end
  end
end

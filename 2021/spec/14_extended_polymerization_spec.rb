# frozen_string_literal: true

require 'extended_polymerization'

describe 'Day 14: Extended Polymerization' do
  let(:test_input) { ExtendedPolymerization.parse_input(File.read('resources/14/test_input.txt')) }
  let(:input) { ExtendedPolymerization.parse_input(File.read('resources/14/input.txt')) }
  context 'Part 1' do
    it 'counts the number of elements in the test input after 10 steps' do
      result = ExtendedPolymerization.count_elements(test_input[:template].chars, test_input[:instructions], 10)
      (_min_element, min_count), (_max_element, max_count) = result.minmax { |(_k1, v1), (_k2, v2)| v1 <=> v2 }
      expect(max_count - min_count).to eq 1588
    end

    it 'counts the number of elements after 10 steps' do
      result = ExtendedPolymerization.count_elements(input[:template].chars, input[:instructions], 10)
      (_min_element, min_count), (_max_element, max_count) = result.minmax { |(_k1, v1), (_k2, v2)| v1 <=> v2 }
      expect(max_count - min_count).to eq 3342
    end
  end

  context 'Part 2' do
    it 'counts the number of elements in the test input after 40 steps' do
      result = ExtendedPolymerization.count_elements(test_input[:template].chars, test_input[:instructions], 40)
      (_min_element, min_count), (_max_element, max_count) = result.minmax { |(_k1, v1), (_k2, v2)| v1 <=> v2 }
      expect(max_count - min_count).to eq 2_188_189_693_529
    end

    it 'counts the number of elements after 40 steps' do
      result = ExtendedPolymerization.count_elements(input[:template].chars, input[:instructions], 40)
      (_min_element, min_count), (_max_element, max_count) = result.minmax { |(_k1, v1), (_k2, v2)| v1 <=> v2 }
      expect(max_count - min_count).to eq 3_776_553_567_525
    end
  end
end

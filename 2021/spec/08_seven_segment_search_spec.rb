# frozen_string_literal: true

require 'seven_segment_search'

describe 'Day 08: Seven Segment Search' do
  context 'Part 1' do
    it 'finds the number of unique digits in the test input' do
      input = SevenSegmentSearch.parse_input(File.read('resources/08/test_input.txt'))
      expect(SevenSegmentSearch.count_unique_digits(input)).to eq 26
    end

    it 'finds the number of unique digits in the input' do
      input = SevenSegmentSearch.parse_input(File.read('resources/08/input.txt'))
      expect(SevenSegmentSearch.count_unique_digits(input)).to eq 412
    end
  end

  context 'Part 2' do
    it 'decodes the signal of the test input' do
      input = SevenSegmentSearch.parse_input(File.read('resources/08/test_input.txt'))
      result = input.sum do |line|
        SevenSegmentSearch.decode_signal(line)
      end
      expect(result).to eq 61_229
    end

    it 'decodes the signal of the test input' do
      input = SevenSegmentSearch.parse_input(File.read('resources/08/input.txt'))
      result = input.sum do |line|
        SevenSegmentSearch.decode_signal(line)
      end
      expect(result).to eq 978_171
    end
  end
end

# frozen_string_literal: true

require 'transparent_origami'

describe 'Day 13: Transparent Origami' do
  let(:test_input) { TransparentOrigami.parse_input(File.read('resources/13/test_input.txt')) }
  let(:input) { TransparentOrigami.parse_input(File.read('resources/13/input.txt')) }

  context 'Part 1' do
    it 'does the thing' do
      result = TransparentOrigami.fold(
        test_input[:points],
        test_input[:instructions].first
      )
      expect(TransparentOrigami.count_visible_points(result)).to eq 17
    end

    it 'does the thing' do
      result = TransparentOrigami.fold(
        input[:points],
        input[:instructions].first
      )
      expect(TransparentOrigami.count_visible_points(result)).to eq 759
    end
  end

  context 'Part 2' do
    let(:test_dot_matrix) { File.read('resources/13/test_result.txt') }
    let(:dot_matrix) { File.read('resources/13/result.txt') }

    it 'does the thing' do
      result = TransparentOrigami.follow_instructions(
        test_input[:points],
        test_input[:instructions]
      )
      expect(TransparentOrigami.draw_points(result)).to eq test_dot_matrix
    end

    it 'does the thing' do
      result = TransparentOrigami.follow_instructions(
        input[:points],
        input[:instructions]
      )
      expect(TransparentOrigami.draw_points(result)).to eq dot_matrix
    end
  end
end

# frozen_string_literal: true

require 'syntax_scoring'

describe 'Day 10: Syntax Scoring' do
  context 'Part 1' do
    it 'returns the correct score for the test data' do
      input = SyntaxScoring.parse_input(File.read('resources/10/test_input.txt'))
      score = SyntaxScoring.score_corrupted_lines(input)
      expect(score).to eq 26_397
    end

    it 'returns the correct score' do
      input = SyntaxScoring.parse_input(File.read('resources/10/input.txt'))
      score = SyntaxScoring.score_corrupted_lines(input)
      expect(score).to eq 318_081
    end
  end

  context 'Part 2' do
    it 'returns the middle score for the test data' do
      input = SyntaxScoring.parse_input(File.read('resources/10/test_input.txt'))
      restored_lines = SyntaxScoring.restore_incomplete_lines(input)
      score = SyntaxScoring.score_restored_lines(restored_lines)
      expect(score).to eq 288_957
    end

    it 'returns the middle score' do
      input = SyntaxScoring.parse_input(File.read('resources/10/input.txt'))
      restored_lines = SyntaxScoring.restore_incomplete_lines(input)
      score = SyntaxScoring.score_restored_lines(restored_lines)
      expect(score).to eq 4_361_305_341
    end
  end
end

# frozen_string_literal: true

require 'bingo'

describe 'Day 04: Giant Squid' do
  context 'Part 1' do
    it 'calculates the correct value for the test input' do
      input = File.read('resources/04/test_input.txt')
      numbers, boards = Bingo.parse_input(input)
      winning_number, winning_board = Bingo.determine_winner(numbers, boards)
      expect(winning_number * winning_board.sum_unmarked).to eq 4512
    end

    it 'calculates the correct value' do
      input = File.read('resources/04/input.txt')
      numbers, boards = Bingo.parse_input(input)
      winning_number, winning_board = Bingo.determine_winner(numbers, boards)
      expect(winning_number * winning_board.sum_unmarked).to eq 28_082
    end
  end

  context 'Part 2' do
    it 'calculates the correct value for the test input' do
      input = File.read('resources/04/test_input.txt')
      numbers, boards = Bingo.parse_input(input)
      winning_number, winning_board = Bingo.determine_last_winner(numbers, boards)
      expect(winning_number * winning_board.sum_unmarked).to eq 1924
    end

    it 'calculates the correct value' do
      input = File.read('resources/04/input.txt')
      numbers, boards = Bingo.parse_input(input)
      winning_number, winning_board = Bingo.determine_last_winner(numbers, boards)
      expect(winning_number * winning_board.sum_unmarked).to eq 8224
    end
  end
end

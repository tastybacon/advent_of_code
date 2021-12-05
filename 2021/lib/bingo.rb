# frozen_string_literal: true

require 'set'

# Methods to solve day 4. Given a file with a set of numbers and some 5x5 bingo
#   boards, determine the winning board.
module Bingo
  def self.parse_input(str)
    lines = str.each_line.map(&:chomp)
    numbers = lines[0].split(',').map(&:to_i)
    boards = parse_boards(lines[2..])
    [numbers, boards]
  end

  def self.parse_boards(board_lines)
    board_lines.slice_after('').map { |x| parse_board(x) }
  end

  def self.parse_board(board_lines)
    Board.new(board_lines.first(5).map { |row| row.split.map(&:to_i) })
  end

  def self.determine_winner(numbers, boards)
    numbers.each do |number|
      boards.each { |b| b.mark_number(number) }
      winner = boards.find(&:bingo?)
      return [number, winner] if winner
    end
  end

  def self.determine_last_winner(numbers, boards)
    numbers.each do |number|
      boards.each { |b| b.mark_number(number) }
      winners, boards = boards.partition(&:bingo?)
      if boards.empty?
        winner = winners.first
        return [number, winner] if winner
      end
    end
  end

  # Represents a 5x5 Bingo board.
  class Board
    ROW_WIN_CONDITIONS = 5.times.map do |column|
      Set.new(Array.new(5) { |i| [i, column] })
    end

    COLUMN_WIN_CONDITIONS = 5.times.map do |row|
      Set.new(Array.new(5) { |i| [row, i] })
    end

    WIN_CONDITIONS = ROW_WIN_CONDITIONS + COLUMN_WIN_CONDITIONS

    def initialize(numbers)
      @numbers = numbers
      @marked = Set.new
    end

    def mark_number(number)
      row_index = @numbers.index { |x| x.include?(number) }
      return false unless row_index

      column_index = @numbers[row_index].index(number)
      @marked << [row_index, column_index]
      true
    end

    def bingo?
      WIN_CONDITIONS.any? { |condition| @marked.superset?(condition) }
    end

    def sum_unmarked
      @numbers.each.with_index.sum do |row, row_index|
        row.each.with_index.sum do |value, column_index|
          if @marked.include?([row_index, column_index])
            0
          else
            value
          end
        end
      end
    end
  end
end

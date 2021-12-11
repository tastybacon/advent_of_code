# frozen_string_literal: true

require 'set'

# Methods for day 10. Provides a couple different ways of scoring
# incomplete/corrupted lines from the navigation subsystem of the
# submarine based on their syntax.
module SyntaxScoring
  MATCHING_CHARACTERS = {
    '(' => ')',
    '[' => ']',
    '{' => '}',
    '<' => '>',
  }.freeze

  START_CHARACTERS = MATCHING_CHARACTERS.keys.to_set.freeze
  END_CHARACTERS = MATCHING_CHARACTERS.values.to_set.freeze

  SYNTAX_ERROR_SCORES = {
    ')' => 3,
    ']' => 57,
    '}' => 1197,
    '>' => 25_137,
  }.freeze

  AUTOCOMPLETE_SCORES = {
    ')' => 1,
    ']' => 2,
    '}' => 3,
    '>' => 4,
  }.freeze

  def self.parse_input(io)
    io.split
  end

  def self.score_corrupted_lines(lines)
    lines.sum { |line| score_corrupted_line(line) }
  end

  def self.score_corrupted_line(line)
    status, char = analyze_line(line)
    if status == :corrupt
      SYNTAX_ERROR_SCORES[char]
    else
      0
    end
  end

  def self.analyze_line(line) # rubocop:disable Metrics/MethodLength
    stack = []
    line.chars.each do |char|
      if START_CHARACTERS.include?(char)
        stack << char
      elsif END_CHARACTERS.include?(char)
        return [:corrupt, char] unless MATCHING_CHARACTERS[stack.last] == char

        stack.pop
        return [:complete] if stack.empty?
      else
        raise "Unrecognized character: '#{char}'"
      end
    end

    [:incomplete, stack]
  end

  def self.restore_incomplete_lines(lines)
    lines.filter_map { |line| restore_incomplete_line(line) }
  end

  # If the given line is incomplete, this method will return a two-element
  # array with the original line as the first element and the characters
  # needed to restore the line as the second element. If the given line is not
  # incomplete (i.e. it is complete or corrupt), this method will return `nil`
  def self.restore_incomplete_line(line)
    status, remaining_characters = analyze_line(line)
    return unless status == :incomplete

    [
      line,
      remaining_characters.reverse.map { |char| MATCHING_CHARACTERS[char] }.join,
    ]
  end

  # Returns the middle score
  def self.score_restored_lines(restored_lines)
    scores = restored_lines.map { |line| score_restored_line(line) }
    scores.sort[scores.length / 2]
  end

  def self.score_restored_line(restored_line)
    restored_characters = restored_line[1]
    restored_characters.chars.reduce(0) do |acc, char|
      (acc * 5) + AUTOCOMPLETE_SCORES[char]
    end
  end
end

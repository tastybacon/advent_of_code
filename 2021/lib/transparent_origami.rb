# frozen_string_literal: true

require 'set'

# Methods to solve day 13, following instructions to fold paper with dots on it
module TransparentOrigami
  def self.parse_input(io)
    io.each_line.with_object({ points: [], instructions: [] }) do |line, acc|
      if line[',']
        acc[:points] << line.chomp.split(',').map(&:to_i)
      elsif line['=']
        extracted = /fold along (?<direction>[xy])=(?<value>\d+)/.match(line)
        acc[:instructions] << [extracted['direction'], extracted['value'].to_i]
      end
    end
  end

  def self.follow_instructions(initial_points, instructions)
    instructions.reduce(initial_points) do |points, instruction|
      fold(points, instruction)
    end
  end

  def self.fold(points, instruction)
    direction, value = instruction
    case direction
    when 'x'
      fold_x(points, value)
    when 'y'
      fold_y(points, value)
    end
  end

  def self.fold_x(points, x_value)
    points.each do |point|
      px = point[0]
      point[0] = (2 * x_value) - px if px > x_value
    end
  end

  def self.fold_y(points, y_value)
    points.each do |point|
      py = point[1]
      point[1] = (2 * y_value) - py if py > y_value
    end
  end

  def self.count_visible_points(points)
    Set.new(points).count
  end

  def self.draw_points(points)
    lines = []
    points.each do |(column, row)|
      lines[row] ||= ''
      lines[row] = lines[row].ljust(column, ' ')
      lines[row][column] = '#'
    end
    lines.join("\n")
  end
end

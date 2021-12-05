# frozen_string_literal: true

# Methods to solve day 1
module SonarSweep
  def self.count_increments(input)
    input.each_cons(2).count { |a, b| b > a }
  end

  def self.count_increments_with_window(input, window_size = 3)
    input.each_cons(window_size).map(&:sum).each_cons(2).count { |a, b| b > a }
  end
end

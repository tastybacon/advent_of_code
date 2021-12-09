# frozen_string_literal: true

require 'set'

# Code for day 8, decoding broken number signals and segmented displays.
module SevenSegmentSearch
  def self.parse_input(io)
    io.each_line.map { |line| line.split('|').map(&:split) }
  end

  def self.count_unique_digits(segment_data)
    segment_data.sum do |segment|
      output = segment[1]
      output.count { |x| [2, 3, 4, 7].include?(x.length) }
    end
  end

  def self.decode_signal(segment_data)
    signal, display_segments = segment_data
    signal = signal.map { |x| x.chars.to_set }
    display_segments = display_segments.map { |x| x.chars.to_set }
    signal_map = determine_signal_meaning(signal)
    segment_map = signal_map.invert
    display_segments.map { |x| segment_map[x].to_s }.join.to_i
  end

  # The following methods determine which scrambled signal corresponds to which
  # value. Some signals depend on previous signals/additional data. I wonder if
  # there's a better/easier way to do this?
  # This also assumes that the input contains only valid signals and enough
  # signals to determine the meaning of all signals and segments.
  def self.determine_signal_meaning(signal) # rubocop:disable Metrics/AbcSize, Metrics/MethodLength
    signals_by_length = signal.group_by(&:length)

    signal_map = {
      1 => find_one(signals_by_length),
      4 => find_four(signals_by_length),
      7 => find_seven(signals_by_length),
      8 => find_eight(signals_by_length),
    }

    signal_map[6] = find_six(signals_by_length, signal_map[1])
    signal_map[3] = find_three(signals_by_length, signal_map[1])

    top_right = (signal_map[8] - signal_map[6]).first
    bottom_right = (signal_map[1] - [top_right]).first

    signal_map[2] = find_two(signals_by_length, top_right, bottom_right)
    signal_map[5] = find_five(signals_by_length, top_right, bottom_right)

    bottom_left = (signal_map[8] - signal_map[5] - [top_right]).first
    signal_map[9] = find_nine(signals_by_length, bottom_left)
    signal_map[0] = find_zero(signals_by_length, signal_map[6], signal_map[9])
    signal_map
  end

  # The signals for 1, 4, 7, and 8 all have unique character lenghts and are easy to find
  def self.find_one(signals_by_length)
    signals_by_length[2].first
  end

  def self.find_four(signals_by_length)
    signals_by_length[4].first
  end

  def self.find_seven(signals_by_length)
    signals_by_length[3].first
  end

  def self.find_eight(signals_by_length)
    signals_by_length[7].first
  end

  # Six is the only signal with length six and does not contain both signals of one
  def self.find_six(signals_by_length, signal_of_one)
    signals_by_length[6].find { |x| !x.superset?(signal_of_one) }
  end

  # Three is the only signal with length five and contains the entire signal of one
  def self.find_three(signals_by_length, signal_of_one)
    signals_by_length[5].find { |x| x.superset?(signal_of_one) }
  end

  def self.find_two(signals_by_length, top_right_segment, bottom_right_segment)
    signals_by_length[5].find do |x|
      x.include?(top_right_segment) && !x.include?(bottom_right_segment)
    end
  end

  def self.find_five(signals_by_length, top_right_segment, bottom_right_segment)
    signals_by_length[5].find do |x|
      !x.include?(top_right_segment) && x.include?(bottom_right_segment)
    end
  end

  def self.find_nine(signals_by_length, bottom_left_segment)
    signals_by_length[6].find { |x| !x.include?(bottom_left_segment) }
  end

  def self.find_zero(signals_by_length, signal_of_six, signal_of_nine)
    signals_by_length[6].find { |x| x != signal_of_six && x != signal_of_nine }
  end
end
